#' @importFrom dplyr tibble bind_rows
#' @importFrom stringr str_c str_pad
#' @importFrom purrr imap map_df
#' @importFrom lubridate `%m+%` months
NULL

#' Originate a new vintage of loans.
#'
#' @description
#' Generates the initial t=1 state for a set of new loans, attaching the
#' product and segment parameters directly to each loan with the correct structure.
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

  loan_ids <- stringr::str_c("L", format(vintage_date, "%Y%m"), stringr::str_pad(1:n_loans, 4, pad = "0"))
  max_amount <- product_params$max_amount

  loan_limits <- round(rnorm(n = n_loans, mean = max_amount * segment_params$avg_limit_pct_of_max, sd = max_amount * 0.05), -2)
  loan_limits <- pmax(pmin(loan_limits, max_amount), product_params$min_amount)

  initial_principals <- round(loan_limits * rnorm(n = n_loans, mean = segment_params$avg_first_balance_pct_of_limit, sd = 0.02), -2)
  initial_principals <- pmin(initial_principals, loan_limits)

  # Create a single parameters list that will be assigned to each loan
  params_list <- list(list(product = product_params, segment = segment_params))

  tibble::tibble(
    loan_id = loan_ids,
    portfolio_name = portfolio_name,
    vintage_date = vintage_date,
    product_name = product_params$product_name,
    segment_name = segment_params$segment_name,
    initial_principal = initial_principals,
    loan_limit = loan_limits,
    initial_term = product_params$contract_duration,
    interest_rate_period = product_params$interest_per_period,
    period_date = vintage_date,
    period = 1,
    opening_balance = initial_principals,
    closing_balance = initial_principals,
    status = "Active",
    # FIX: Assign the correctly structured list to each loan
    params = rep(params_list, n_loans)
  )
}

#' Generate multiple loan vintages from a list of sales plans.
#'
#' @param sales_plans A named list of sales plans.
#' @param portfolio_name A string to name the resulting portfolio.
#' @return A single tibble containing the initial state of all generated loans.
#' @export
generate_sales <- function(sales_plans, portfolio_name) {
  purrr::map_df(sales_plans, function(plan) {
    list_of_vintages <- purrr::imap(plan$monthly_loans, function(n_loans, idx) {
      current_vintage_date <- plan$vintage_date %m+% months(idx - 1)
      originate_loans(
        vintage_date = current_vintage_date,
        n_loans = n_loans,
        product_params = plan$product_params,
        segment_params = plan$segment_params,
        portfolio_name = portfolio_name
      )
    })
    dplyr::bind_rows(list_of_vintages)
  })
}
