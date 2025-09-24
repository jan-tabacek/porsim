#' @importFrom dplyr %>% mutate if_else select filter bind_rows
#' @importFrom purrr map2_dbl
#' @importFrom stats runif
NULL

#' Handle the initiation and resolution of defaults.
#'
#' @description
#' This is a vectorized function that applies default and debt sale logic. It
#' splits the portfolio by status to perform efficient, column-wise operations.
#'
#' @param loans_to_process A tibble of loans representing the current period's state.
#' @return A tibble with updated status for defaulted or sold loans.
#'
handle_default <- function(loans_to_process) {

  # --- 1. Split data by status ---
  active_loans_idx <- which(loans_to_process$status == "Active")
  defaulted_loans_idx <- which(loans_to_process$status == "Defaulted")

  active_loans <- loans_to_process[active_loans_idx, ]
  defaulted_loans <- loans_to_process[defaulted_loans_idx, ]

  # --- 2. Process ACTIVE loans: check for new defaults ---
  if (nrow(active_loans) > 0) {
    # Vectorized calculation of default probability for each active loan
    default_probs <- purrr::map2_dbl(
      active_loans$params,
      active_loans$period,
      ~ .x$segment$default_prob[min(.y, length(.x$segment$default_prob))]
    )

    active_loans <- active_loans %>%
      dplyr::mutate(
        will_default = runif(n()) < default_probs,
        # Update state for loans that are now defaulting
        status = dplyr::if_else(will_default, "Defaulted", status),
        periods_in_default = dplyr::if_else(will_default, 1, periods_in_default),
        closing_balance = dplyr::if_else(will_default, opening_balance, closing_balance),
        payment = dplyr::if_else(will_default, 0, payment),
        principal_paid = dplyr::if_else(will_default, 0, principal_paid),
        interest_paid = dplyr::if_else(will_default, 0, interest_paid)
      ) %>%
      dplyr::select(-will_default)

    # Place the modified loans back into the main data frame
    loans_to_process[active_loans_idx, ] <- active_loans
  }

  # --- 3. Process DEFAULTED loans: check for debt sales ---
  if (nrow(defaulted_loans) > 0) {
    defaulted_loans <- defaulted_loans %>%
      dplyr::mutate(
        periods_in_default = periods_in_default + 1,
        # Vectorized extraction of parameters
        time_till_accel = purrr::map_dbl(params, ~ .x$product$time_periods_till_acceleration),
        recovery_rate = purrr::map_dbl(params, ~ .x$segment$debt_sale_recovery_rate),

        will_be_sold = periods_in_default >= time_till_accel,

        # Update state for loans being sold
        status = dplyr::if_else(will_be_sold, "Sold", status),
        debt_sale_proceeds = dplyr::if_else(will_be_sold, opening_balance * recovery_rate, debt_sale_proceeds),
        closing_balance = dplyr::if_else(will_be_sold, 0, closing_balance)
      ) %>%
      dplyr::select(-time_till_accel, -recovery_rate, -will_be_sold)

    # Place the modified loans back into the main data frame
    loans_to_process[defaulted_loans_idx, ] <- defaulted_loans
  }

  return(loans_to_process)
}
