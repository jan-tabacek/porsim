#' @importFrom dplyr %>% mutate if_else select
#' @importFrom purrr map2_dbl
#' @importFrom stats runif
NULL

#' (Placeholder) Handle extra, unscheduled payments.
handle_extra_payment <- function(loans_to_process) {
  # This is a placeholder for future functionality.
  return(loans_to_process)
}


#' Handle early, full repayments for active loans.
#'
#' @description
#' This function checks if any "Active" loans will repay their entire
#' balance early. This is a vectorized version that is more performant than
#' a rowwise implementation.
#'
#' @param loans_to_process A tibble of loans representing the current period's state.
#' @return A tibble with updated status for loans that were repaid early.
#'
handle_early_repayment <- function(loans_to_process) {

  # Isolate the active loans to avoid unnecessary calculations
  active_loans_idx <- which(loans_to_process$status == "Active")
  if (length(active_loans_idx) == 0) {
    return(loans_to_process)
  }
  active_loans <- loans_to_process[active_loans_idx, ]

  # --- 1. Vectorized calculation of repayment probability ---
  # Use purrr::map2_dbl to iterate over two columns in parallel
  repayment_probs <- purrr::map2_dbl(
    active_loans$params,
    active_loans$period,
    ~ .x$segment$early_repayment_rate[min(.y, length(.x$segment$early_repayment_rate))]
  )

  active_loans <- active_loans %>%
    dplyr::mutate(
      will_repay_early = runif(n()) < repayment_probs,

      # --- 2. Update state for loans that repay early ---
      proper_payment_interest = dplyr::if_else(will_repay_early, opening_balance * interest_rate_period, interest_paid),
      principal_paid = dplyr::if_else(will_repay_early, opening_balance, principal_paid),
      interest_paid = dplyr::if_else(will_repay_early, proper_payment_interest, interest_paid),
      payment = dplyr::if_else(will_repay_early, principal_paid + interest_paid, payment),
      closing_balance = dplyr::if_else(will_repay_early, 0, closing_balance),
      status = dplyr::if_else(will_repay_early, "Paid_Off_Early", status)
    ) %>%
    dplyr::select(-will_repay_early, -proper_payment_interest)

  # Place the modified active loans back into the original data frame
  loans_to_process[active_loans_idx, ] <- active_loans

  return(loans_to_process)
}
