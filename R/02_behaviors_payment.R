#' @importFrom dplyr %>% mutate if_else
NULL

#' Handle scheduled, proper payments for active loans.
#'
#' @description
#' This function calculates the standard monthly payment for all loans with an
#' "Active" status, ensuring the integrity of all data columns.
#'
#' @param loans_to_process A tibble of loans representing the current period's state.
#' @return A tibble with updated payment details and status.
#'
handle_proper_payment <- function(loans_to_process) {

  loans_to_process %>%
    dplyr::mutate(
      # --- Create temporary calculation columns ---
      # These will only have non-NA values for Active loans
      remaining_term_calc = dplyr::if_else(status == "Active", initial_term - period + 1, NA_real_),

      payment_calc = dplyr::if_else(
        status == "Active",
        calculate_pmt(opening_balance, interest_rate_period, remaining_term_calc),
        payment # Keep existing value if not active
      ),

      interest_paid_calc = dplyr::if_else(
        status == "Active",
        opening_balance * interest_rate_period,
        interest_paid # Keep existing value if not active
      ),

      principal_paid_calc = dplyr::if_else(
        status == "Active",
        payment_calc - interest_paid_calc,
        principal_paid # Keep existing value if not active
      ),

      closing_balance_calc = dplyr::if_else(
        status == "Active",
        opening_balance - principal_paid_calc,
        closing_balance # Keep existing value if not active
      ),

      closing_balance_calc = dplyr::if_else(closing_balance_calc < 0.01, 0, closing_balance_calc), # Keep existing value if not active

      status_calc = dplyr::if_else(
        status == "Active" & closing_balance_calc < 0.01,
        "Paid_Off",
        status # Keep existing value if not paid off
      ),

      # --- Overwrite the original columns with the calculated values ---
      payment = payment_calc,
      interest_paid = interest_paid_calc,
      principal_paid = principal_paid_calc,
      closing_balance = closing_balance_calc,
      status = status_calc
    ) %>%
    # --- Remove the temporary columns ---
    dplyr::select(-dplyr::ends_with("_calc"))
}


#' (Placeholder) Handle extra, unscheduled payments.
#'
handle_extra_payment <- function(loans_to_process) {
  # This is a placeholder for future functionality.
  return(loans_to_process)
}
