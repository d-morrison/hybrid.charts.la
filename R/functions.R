#NA functions

#LF trick  function
nudge_zero <- function(x){
  if(identical(x,0)){
    x <- 0.1
  }
  return(x)
}

#function to do NA conversion
zero_NA <- function(x){
  x[x == 0] <- NA
  x
}

#functions from July 2020 to force monotonicity starting from cum series
make_vec1 <- function(x){
  x_out <- x -lag(x)
  
  x_out[1] <- x[1]
  
  return(x_out)
}

# A function factory for getting integer y-axis values.
# from: https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#function to find index that marks first sequence of length_use values.  Default length = 8 per Lloyd Provost 30 March 2020 
index_test <- function(x,
                       index,
                       length_use=8){
  x_check <- x[index:(index+length_use - 1)]
  if(all(x_check>0)){
    use_seq <- TRUE
    index_use <- index
  } else {
    use_seq <- FALSE
    index_use <- index + 1
  }
  return(list(use_seq,index_use))
}

#function to detect large data dumps
detect_outlier_dates <- function(
  data,
  ratio_limit = 6)
{
  model <- loess(New_var ~ as.numeric(datex), data = data, span = 0.25)
  
  
  outlier_index <- data$New_var > max(10, median(data$New_var, na.rm = TRUE)) &
    (data$New_var / model$fitted) >= ratio_limit
  
  data$datex[outlier_index]
}

#function to force monotonicity in nominally monotone non-decreasing series
force_monotonicity <- function(x) {
  
  sum_original <- sum(x, na.rm = TRUE)
  
  # spread negative values backwards in series so that cumulative sum never decreases
  negative_index <- which(x < 0)
  
  while (any(negative_index)) {
    
    index <- tail(negative_index, 1)
    
    value <- abs(x[index])
    
    transferred <- 0
    
    x[index] <- 0
    
    while (transferred < value) {
      
      index <- index - 1
      
      transfer <- min(x[index], value - transferred)
      
      x[index] <- x[index] - transfer
      
      transferred <- transferred + transfer
    }
    
    negative_index <- which(x < 0)
  }
  
  stopifnot(sum(x, na.rm = TRUE) == sum_original)
  
  x
}

