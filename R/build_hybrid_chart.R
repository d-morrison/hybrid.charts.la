
#' build hybrid chart
#'
#' @param data 
#' @param community 
#' @param adjust 
#'
#' @return
#' @export
#'
build_hybrid_chart = function(
  data,
  community,
  adjust = FALSE
)
{
  
  chart_data = 
    data %>% 
    filter(name == community) %>%
    mutate(var_cum = confirmed_cases) %>%
    arrange(date) %>% 
    filter(!is.na(var_cum)) %>% 
    mutate(
      New_var = force_monotonicity(c(var_cum[1], diff(var_cum))),
      New_var_max = max(New_var, na.rm = TRUE)) %>%
    rename(datex = date) %>% 
    head(., -1) %>%
    find_phase_dates(adjust = adjust)
  
  p1 = plotly::ggplotly(graph_function(chart_data, adjust = adjust))
}
