#' @importFrom dplyr %>% mutate if_else select rowwise ungroup
#' @importFrom stats runif
NULL

#' Handle the initiation and resolution of defaults.
#'
#' @param loans_to_process A tibble of loans representing the current period's state.
#' @return A tibble with updated status for defaulted or sold loans.
#'
handle_default <- function(loans_to_process) {

  if (nrow(loans_to_process) == 0) {
    return(loans_to_process)
  }

  loans_to_process %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Create the probability column
      default_prob_this_period = {
        if (status == "Active") {
          prob_vec <- params$segment$default_prob
          prob_vec[min(period, length(prob_vec))]
        } else {
          NA_real_
        }
      }
    ) %>%
    dplyr::mutate(
      will_default = !is.na(default_prob_this_period) && runif(1) < default_prob_this_period,

      # FIX: First, increment the counter for loans already in default.
      # This ensures the column is properly handled before being overwritten for new defaults.
      periods_in_default = dplyr::if_else(status == "Defaulted", periods_in_default + 1, periods_in_default),

      # Now, set the counter to 1 for any loans that just defaulted this period.
      periods_in_default = dplyr::if_else(will_default, 1, periods_in_default),

      # Update state for loans that just defaulted
      status = dplyr::if_else(will_default, "Defaulted", status),
      closing_balance = dplyr::if_else(will_default, opening_balance, closing_balance),
      payment = dplyr::if_else(will_default, 0, payment),
      principal_paid = dplyr::if_else(will_default, 0, principal_paid),
      interest_paid = dplyr::if_else(will_default, 0, interest_paid),

      # Transition: Defaulted -> Sold
      will_be_sold = status == "Defaulted" & periods_in_default >= params$product$time_periods_till_acceleration,

      # Update state for loans that are sold this period
      status = dplyr::if_else(will_be_sold, "Sold", status),
      debt_sale_proceeds = dplyr::if_else(will_be_sold, opening_balance * params$segment$debt_sale_recovery_rate, debt_sale_proceeds),
      closing_balance = dplyr::if_else(will_be_sold, 0, closing_balance)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-default_prob_this_period, -will_default, -will_be_sold)
}
