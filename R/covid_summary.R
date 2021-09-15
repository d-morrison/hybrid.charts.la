#' covid_summary
#'
#' summarizes across all places
#'
#' @param data_frame 
#'
#' @return
#' @export
#'
covid_summary <- function(data_frame){
  data_frame %>%
    group_by(date) %>%
    summarize(
      .groups = 'drop',
      across(
        all_of(c(
          "population",
          "confirmed_cases",
          "new_cases"
          # "new_cases_in_last_14_days" # don't do this, introduces 0s instead of NAs at start
        )),
        function(x) sum(x, na.rm = TRUE)
      )) %>%
    mutate(
      "new_cases_in_last_14_days" = 
        confirmed_cases - dplyr::lag(confirmed_cases, 14),
      "pct_infected_cumulative" = 
        100 * confirmed_cases / population,
      "pct_new_cases_in_last_14_days" = 
        100 * new_cases_in_last_14_days/population,
      "var_cum" = confirmed_cases)
}