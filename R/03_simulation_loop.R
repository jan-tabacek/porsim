#' @importFrom dplyr %>% group_by filter ungroup any_of bind_rows arrange last
#' @importFrom lubridate %m+% months
NULL

#' Run a simulation for a portfolio over time.
#'
#' @description
#' This function simulates a portfolio's performance over time. It uses each
#' loan's `period` as the primary calendar, calculating the `period_date` by
#' adding months to the loan's original `vintage_date`.
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

  live_loans <- initial_portfolio %>%
    dplyr::mutate(
      periods_in_default = 0,
      debt_sale_proceeds = 0
    )

  while(nrow(live_loans) > 0) {

    # 1. Prepare all live loans for processing in the next period.
    loans_to_process <- live_loans %>%
      dplyr::mutate(
        period = period + 1,
        opening_balance = closing_balance,
        # FIX: Calculate period_date based on each loan's own vintage and period
        # period_date = vintage_date %m+% months(period - 1),
        period_date = ceiling_date(floor_date(vintage_date, "month") %m+% months(period - 1), "month") - days(1),
        payment = 0, principal_paid = 0, interest_paid = 0, debt_sale_proceeds = 0
      )

    # 2. Apply the pipeline of behaviors.
    processed_loans <- loans_to_process %>%
      handle_top_up() %>%
      handle_default() %>%
      handle_early_repayment() %>%
      handle_extra_payment() %>%
      handle_proper_payment()

    # 3. Append the results of this period to the master history.
    if (nrow(processed_loans) > 0) {
      full_history <- dplyr::bind_rows(full_history, processed_loans)
    } else {
      break # Safety break
    }

    # 4. Update the set of live_loans for the next iteration.
    live_loans <- processed_loans %>%
      dplyr::filter(status %in% c("Active", "Defaulted"))
  }

  dplyr::arrange(full_history, loan_id, period)
}
