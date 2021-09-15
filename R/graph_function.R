
#' Title
#'
#' function to graph 
#' @param data 
#'
#' @return
#' @export
#'

graph_function <- function(
  data,
  adjust = FALSE,
  type = if_else(adjust, "Adjusted", "Raw"),
  var_name = "Confirmed Cases")
{
  g <- ggplot(
    data,
    aes(
      x = datex,
      y = New_var)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    geom_line(
      aes(
        y = midline),
      color = 'red') +
    geom_line(
      aes(
        y = ucl),
      color = 'blue',
      linetype = 'dashed') +
    geom_line(
      aes(
        y = lcl),
      color = 'blue',
      linetype = 'dashed') +
    # geom_point(
    #   aes(
    #     y = New_var_Dump),
    #   color = 'red') +
    scale_y_continuous(
      limits = c(
        0, 
        max(
          20, 
          2 * max(data$New_var, na.rm = TRUE), 
          max(data$New_var_Dump, na.rm = TRUE)))) +
    scale_x_date(date_labels = "%b %d", breaks = "2 weeks") +
    theme_minimal() +
    labs(
      x = "",
      y = paste(type, var_name),
      title = paste("SPC Chart for", type, var_name)) + 
    theme(axis.text.x = element_text(angle = -45, vjust = 0.2, hjust=0.5))
  
  
  return(plotly::ggplotly(g))
}
