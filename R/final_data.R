

#' final_data
#'
#' create data frame for confirmed cases to be put into the phase determination function
#' @param data_frame 
#'
#' @return
#' @export
#'

final_data <- function(data_frame){
  data_frame %>% 
    arrange(date) %>% 
    filter(!is.na(var_cum)) %>% 
    mutate(New_var = force_monotonicity(c(var_cum[1],diff(var_cum))),
           New_var_max = max(New_var, na.rm = TRUE)) %>%
    rename(datex = date)
}
