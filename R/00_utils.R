#' Calculate the periodic payment for an annuity.
#'
#' @param balance The outstanding principal balance.
#' @param rate The interest rate per period.
#' @param n_periods The number of remaining periods.
#' @return The periodic payment amount.
#'
calculate_pmt <- function(balance, rate, n_periods) {

  # Use ifelse for vectorized conditional logic.
  # This now correctly handles the case where some loans in the vector
  # might have a zero interest rate.
  payment <- ifelse(
    rate == 0,
    balance / n_periods,
    balance * (rate * (1 + rate)^n_periods) / ((1 + rate)^n_periods - 1)
  )

  # Return a positive value for payment
  return(abs(payment))
}


extend_or_truncate <- function(x, n) {
  if (length(x) >= n) {
    # truncate
    x[1:n]
  } else {
    # extend by repeating the last element
    c(x, rep(tail(x, 1), n - length(x)))
  }
}
