
#' Title
#' function to separate the raw data
#' @param data_all 
#'
#' @return
#' @export
#'

raw_data <- function(data_all)
{
  df_raw <- data_all[data_all$var_name == "Confirmed Cases" & data_all$type == "Raw",]
  # df_var <- d2[d2$var_name == "confirmed_cases" & d2$type == "raw",]
}