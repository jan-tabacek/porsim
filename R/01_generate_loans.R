#' @importFrom dplyr tibble bind_rows
#' @importFrom stringr str_c str_pad
#' @importFrom purrr imap map_df
#' @importFrom lubridate `%m+%` months
NULL

#' Originate a new vintage of loans.
#'
#' @description
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

  if (n_loans == 0) {
    return(tibble::tibble())
  }

  # 1. Generate unique loan IDs
  loan_ids <- stringr::str_c(
    "L",
    format(vintage_date, "%Y%m"),
    stringr::str_pad(1:n_loans, 4, pad = "0")
  )

  # 2. Calculate loan-specific characteristics based on averages
  max_amount <- product_params$max_amount

  loan_limits <- rnorm(
    n = n_loans,
    mean = max_amount * segment_params$avg_limit_pct_of_max,
    sd = max_amount * 0.05
  )
  loan_limits <- round(loan_limits, -2)

  loan_limits <- pmax(pmin(loan_limits, max_amount), product_params$min_amount)

  initial_principals <- loan_limits * rnorm(
    n = n_loans,
    mean = segment_params$avg_first_balance_pct_of_limit,
    sd = 0.02
  )
  initial_principals <- round(initial_principals, -1)

  initial_principals <- pmin(initial_principals, loan_limits)

  # 3. Assemble the initial state tibble
  tibble::tibble(
    loan_id = loan_ids,
    portfolio_name = portfolio_name,
    vintage_date = vintage_date,
    product_name = product_params$product_name,
    segment_name = "standard_payer",
    initial_principal = initial_principals,
    loan_limit = loan_limits,
    initial_term = product_params$contract_duration,
    interest_rate_period = product_params$interest_per_period,
    period_date = vintage_date,
    period = 1,
    opening_balance = initial_principals,
    closing_balance = initial_principals,
    principal_paid = 0,
    interest_paid = 0,
    payment = 0,
    status = "Active"
  )
}


#' Generate multiple loan vintages from a list of sales plans.
#'
#' @description
#' A wrapper around `originate_loans` to create a complete initial portfolio
#' from a structured list of one or more sales plans.
#'
#' @param sales_plans A named list of sales plans. See example for structure.
#' @param portfolio_name A string to name the resulting portfolio.
#'
#' @return A single tibble containing the initial state of all generated loans
#'   from all sales plans.
#'
#' @export
#' @examples
#' product_mtp <- list(
#'   product_name = "MTP",
#'   min_amount = 1,
#'   max_amount = 1000,
#'   contract_duration = 16,
#'   interest_per_period = 0.022
#' )
#' segment_standard <- list(
#'   avg_limit_pct_of_max = 0.80,
#'   avg_first_balance_pct_of_limit = 0.95
#' )
#' plans <- list(
#'   plan_A = list(
#'     product_params = product_mtp,
#'     segment_params = segment_standard,
#'     monthly_loans = c(10, 20),
#'     vintage_date = as.Date("2025-01-31")
#'   )
#' )
#' generate_sales(plans, "MyPortfolio")
generate_sales <- function(sales_plans, portfolio_name) {

  # Use map_df to iterate over each plan in the main list
  purrr::map_df(sales_plans, function(plan) {

    # Use imap to create a LIST of data frames (one for each month's vintage)
    list_of_vintages <- purrr::imap(plan$monthly_loans, function(n_loans, idx) {

      # The first vintage (idx=1) starts on the given date.
      # Subsequent vintages are offset by months.
      current_vintage_date <- plan$vintage_date %m+% months(idx - 1)

      originate_loans(
        vintage_date = current_vintage_date,
        n_loans = n_loans,
        product_params = plan$product_params,
        segment_params = plan$segment_params,
        portfolio_name = portfolio_name
      )
    })

    # Combine the list of vintages into a single data frame for this plan
    dplyr::bind_rows(list_of_vintages)
  })
}
