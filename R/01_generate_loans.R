library(tidyverse)
library(stringr)

#' Originate a new vintage of loans.
#'
#' Generates the initial t=1 state for a set of new loans based on
#' product and segment parameters.
#'
#' @param vintage_date The origination date for this vintage of loans.
#' @param n_loans The number of new loans to generate.
#' @param product_params A list of parameters defining the product.
#' @param segment_params A list of parameters defining the client segment.
#' @param portfolio_name A string naming the portfolio.
#' @return A tibble representing the initial state of the new loans.
#'
originate_loans <- function(vintage_date, n_loans, product_params, segment_params, portfolio_name) {

  # 1. Generate unique loan IDs
  loan_ids <- str_c(
    "L",
    format(vintage_date, "%Y%m"),
    str_pad(1:n_loans, 4, pad = "0")
  )

  # 2. Calculate loan-specific characteristics based on averages
  # We use rnorm to create some variance around the provided averages.
  max_amount <- product_params$max_amount

  loan_limits <- rnorm(
    n = n_loans,
    mean = max_amount * segment_params$avg_limit_pct_of_max,
    sd = max_amount * 0.05 # Assume a 5% standard deviation for variation
  )

  # Ensure limit is within product min/max
  loan_limits <- pmax(pmin(loan_limits, max_amount), product_params$min_amount)

  initial_principals <- round(
      loan_limits * rnorm(
        n = n_loans,
        mean = segment_params$avg_first_balance_pct_of_limit,
        sd = 0.02 # Assume a small 2% SD for variation
      ),
      -2
  )

  # Ensure balance does not exceed the limit
  initial_principals <- pmin(initial_principals, loan_limits)

  # 3. Assemble the initial state tibble
  new_vintage_df <- tibble(
    # --- Identifiers and Static Characteristics ---
    loan_id = loan_ids,
    portfolio_name = portfolio_name,
    vintage_date = vintage_date,
    product_name = product_params$product_name,
    segment_name = "standard_payer",
    initial_principal = initial_principals,
    loan_limit = loan_limits,
    initial_term = product_params$contract_duration,
    interest_rate_period = product_params$interest_per_period,

    # --- Initial Time-Varying State (for period 1) ---
    period_date = vintage_date,
    period = 1,
    opening_balance = initial_principals,
    closing_balance = initial_principals, # Closing balance is same as opening for t=1
    principal_paid = 0,
    interest_paid = 0,
    payment = 0,
    status = "Active"
  )

  return(new_vintage_df)
}
