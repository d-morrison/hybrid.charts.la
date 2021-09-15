

#' Title
#'
#' find phase dates for both raw and adjusted data
#' @param data_all 
#'
#' @return
#' @export
#'

phase_data <- function(data_all)
  {
  
  data_all_raw <- by(
    data = data_all,
    INDICES = data_all[c('var_name')],
    FUN = find_phase_dates,
    #ghost = TRUE,
    adjust = FALSE)
  
  data_all_raw <- do.call(dplyr::bind_rows, data_all_raw)
  
  data_all_raw$type <- 'Raw'
  
  data_all_adjusted <- by(
    data = data_all,
    INDICES = data_all[c('var_name')],
    FUN = find_phase_dates,
    #ghost = TRUE,
    adjust = TRUE)
  
  
  data_all_adjusted <- do.call(dplyr::bind_rows, data_all_adjusted)
  
  data_all_adjusted$type <- 'Adjusted'
  
  data_all <- do.call(dplyr::bind_rows, list(data_all_raw, data_all_adjusted))
  # d2 <- data_all %>% mutate(New_var_Dump = as.numeric(New_var_Dump))
}