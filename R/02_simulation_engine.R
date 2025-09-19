library(tidyverse)

#' Process one time period for a set of active loans.
#'
#' This is a helper function for `run_simulation`. It calculates payments,
#' interest, and principal for a single period, updating the loan's state.
#'
#' @param active_loans_df A tibble of loans with status "Active".
#' @param period_date The date of the current simulation period.
#' @return A tibble with the updated state for the given period.
#'
process_period <- function(active_loans_df, period_date) {

  if (nrow(active_loans_df) == 0) {
    return(tibble())
  }

  # Calculate remaining term for each loan
  updated_df <- active_loans_df %>%
    mutate(
      remaining_term = initial_term - period + 1
    )

  # Calculate the scheduled payment for this period
  updated_df <- updated_df %>%
    mutate(
      payment = calculate_pmt(
        balance = opening_balance,
        rate = interest_rate_period,
        n_periods = remaining_term
      )
    )

  # Calculate interest and principal paid
  updated_df <- updated_df %>%
    mutate(
      interest_paid = opening_balance * interest_rate_period,
      principal_paid = payment - interest_paid,
      closing_balance = opening_balance - principal_paid
    )

  # Update status for loans that are now paid off
  # Add a small tolerance for floating point rounding issues
  updated_df <- updated_df %>%
    mutate(
      status = if_else(closing_balance < 0.01, "Paid_Off", "Active")
    )

  # Finalize columns for this period's record
  updated_df %>%
    mutate(period_date = period_date) %>%
    select(
      # Identifiers
      loan_id, portfolio_name, vintage_date, product_name, segment_name,
      initial_principal, loan_limit, initial_term, interest_rate_period,
      # State for this period
      period_date, period, opening_balance, payment, principal_paid,
      interest_paid, closing_balance, status
    )
}


#' Run a simulation for a portfolio over time.
#'
#' Takes an initial portfolio state (the output of `originate_loans`)
#' and simulates its performance month by month.
#'
#' @param initial_portfolio A tibble from `originate_loans`.
#' @return A single tibble with the full history of all loans.
#'
run_simulation <- function(initial_portfolio) {

  # The first period's history is the initial state itself
  full_history <- list(initial_portfolio)

  # The loans that are active at the start of period 2
  active_loans_for_next_period <- initial_portfolio %>%
    filter(status == "Active") %>%
    mutate(
      period = period + 1,
      opening_balance = closing_balance
    )

  simulation_date <- initial_portfolio$period_date[1]

  while(nrow(active_loans_for_next_period) > 0) {

    # Advance the simulation clock by one month
    simulation_date <- simulation_date %m+% months(1)

    # Process the state changes for the current period
    current_period_history <- process_period(
      active_loans_df = active_loans_for_next_period,
      period_date = simulation_date
    )

    # Add this period's results to our master list
    full_history <- append(full_history, list(current_period_history))

    # Prepare the set of loans for the *next* period
    active_loans_for_next_period <- current_period_history %>%
      filter(status == "Active") %>%
      mutate(
        period = period + 1,
        opening_balance = closing_balance
      )
  }

  # Combine all the monthly history tibbles into one large tibble
  bind_rows(full_history) %>%
    arrange(loan_id, period)
}
