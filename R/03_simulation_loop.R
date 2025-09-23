#' @importFrom dplyr %>% group_by filter ungroup any_of bind_rows arrange last
#' @importFrom lubridate %m+% months ceiling_date
NULL

#' Run a simulation for a portfolio over time.
#'
#' @param initial_portfolio A tibble from `generate_sales`.
#' @return A single tibble with the full history of all loans.
#' @export
run_simulation <- function(initial_portfolio) {

  full_history <- initial_portfolio %>%
    dplyr::mutate(
      payment = 0, principal_paid = 0, interest_paid = 0,
      periods_in_default = 0, debt_sale_proceeds = 0
    )

  # FIX: Initialize live_loans with all necessary state-tracking columns.
  live_loans <- initial_portfolio %>%
    dplyr::mutate(
      periods_in_default = 0,
      debt_sale_proceeds = 0
    )

  while(nrow(live_loans) > 0) {

    current_period_date <- min(live_loans$period_date)
    current_period_date <- lubridate::ceiling_date(current_period_date, "month") %m+% months(1) - 1

    message("Processing ", current_period_date)

    loans_to_process <- live_loans %>%
      dplyr::mutate(
        period = period + 1,
        opening_balance = closing_balance,
        period_date = current_period_date,
        payment = 0, principal_paid = 0, interest_paid = 0, debt_sale_proceeds = 0
      )

    processed_loans <- loans_to_process %>%
      handle_top_up() %>%
      handle_default() %>%
      handle_extra_payment() %>%
      handle_proper_payment()

    if (nrow(processed_loans) > 0) {
      full_history <- dplyr::bind_rows(full_history, processed_loans)
    } else {
      break
    }

    live_loans <- processed_loans %>%
      dplyr::filter(status %in% c("Active", "Defaulted"))
  }

  dplyr::arrange(full_history, loan_id, period)
}
