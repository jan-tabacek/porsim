#' Calculate the periodic payment for an annuity.
#'
#' @param balance The outstanding principal balance.
#' @param rate The interest rate per period.
#' @param n_periods The number of remaining periods.
#' @return The periodic payment amount.
#'
calculate_pmt <- function(balance, rate, n_periods) {
  # Handle the case where there is no interest.
  if (rate == 0) {
    return(balance / n_periods)
  }
  
  # Standard annuity payment formula
  payment <- balance * (rate * (1 + rate)^n_periods) / ((1 + rate)^n_periods - 1)
  
  # Return a positive value for payment
  return(abs(payment))
}