#' Model phase changes
#' function to identify changes in phase
#' @param data 
#' @param y 
#'
#' @return
#' @export
#'
model_phase_change <- function(
  data,
  y)
{
  data[[y]][!is.na(data[[y]]) & data[[y]] <= 0] <- NA
  
  data$log_10_var <- log10(data[[y]])
  
  data$log_10_var[is.infinite(data$log_10_var)] <- NA
  
  data$serial_day <- as.numeric(difftime(data$datex, min(data$datex, na.rm = TRUE), units = 'days')) + 1
  
  repeat {
    
    lm_raw <- try(lm(log_10_var ~ serial_day,
                     data = data))
    
    if ('try-error' %in% class(lm_raw)) break
    else {
      residual_diff <- diff(lm_raw$residuals)
      
      median_moving_range <- median(abs(residual_diff), na.rm=TRUE)
      
      data <- merge(data,
                    cbind(serial_day = lm_raw$model$serial_day,
                          fitted_value = lm_raw$fitted.values),
                    by = 'serial_day',
                    all.x = TRUE)
      
      distance <- abs(data$log_10_var - data$fitted_value) / median_moving_range
      
      data$fitted_value <- NULL
      
      break
    }
  }
  
  if ('try-error' %in% class(lm_raw)) {
    
    output <- list(
      lm = NULL,
      exp_growth = FALSE,
      median_moving_range = NULL)
    
  } else {
    
    serial_day_coefficient <- lm_raw$coefficients['serial_day']
    
    growth_sign <- sign(serial_day_coefficient)
    
    serial_day_pvalue <- try(summary(lm_raw)$coefficients['serial_day', 'Pr(>|t|)'])
    
    exp_growth <- !is.na(serial_day_pvalue) &&
      serial_day_pvalue < 0.05
    
    mean_var <- mean(data$log_10_var, na.rm = TRUE)
    
    # If exponential growth (up or down) is not detected, 
    # re-define median moving range based on mean_var as midline
    if (!exp_growth) {
      
      residuals_mean <- data$log_10_var - mean_var
      
      median_moving_range <- median(abs(residuals_mean), na.rm=TRUE)
      
    }
    
    output <- list(
      lm = lm_raw,
      mean_var = mean_var,
      growth_sign = growth_sign,
      exp_growth = exp_growth,
      median_moving_range = median_moving_range)
  }
  
  output
}
