#' adj_data
#'
#' @param data_all 
#'
#' @return
#' @export
#'

adj_data <- function(data_all){
  df_adj <- data_all[data_all$var_name == "Confirmed Cases" & data_all$type == "Adjusted",]
  # df_var <- d2[d2$var_name == "confirmed_cases" & d2$type == "adjusted",] 
}