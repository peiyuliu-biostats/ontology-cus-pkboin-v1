fun_numeric_check_warning <- function(input_value, lower_bound = NULL, upper_bound = NULL, 
                                      lower_bound_warning = FALSE, upper_bound_warning = FALSE, 
                                      check_int = FALSE)
{
  tmp <- as.numeric(input_value)
  show_warning <- FALSE
  warning_message <- NA
  
  lower_bound <- as.numeric(lower_bound)
  # lower_bound either non-numeric or missing
  if(length(lower_bound) == 0) {
    tmp_lower_bound <- -Inf
  } else if(is.na(lower_bound)) {
    tmp_lower_bound <- -Inf
  } else {
    tmp_lower_bound <- lower_bound
  }
  
  upper_bound <- as.numeric(upper_bound)
  # upper_bound either non-numeric or missing
  if(length(upper_bound) == 0) {
    tmp_upper_bound <- Inf
  } else if(is.na(upper_bound)) {
    tmp_upper_bound <- Inf
  } else {
    tmp_upper_bound <- upper_bound
  }
  
  if(is.na(tmp)) {
    show_warning <- TRUE 
    warning_message <- "Incorrect input!"
  } else if(tmp < tmp_lower_bound) {
    show_warning <- TRUE 
    warning_message <- "Too small input!"
  } else if(tmp > tmp_upper_bound) {
    show_warning <- TRUE
    warning_message <- "Too large input!"
  } else if(lower_bound_warning == TRUE & tmp_lower_bound > -Inf & 
            tmp == tmp_lower_bound) {
    show_warning <- TRUE
    warning_message <- "Too small input!"
  } else if(upper_bound_warning == TRUE & tmp_upper_bound < Inf & 
            tmp == tmp_upper_bound) {
    show_warning <- TRUE
    warning_message <- "Too large input!"
  } else if(check_int == TRUE & !tmp%%1 == 0) {
    show_warning <- TRUE
    warning_message <- "Please enter an integer!"
  }
  
  return(list(show_warning = show_warning, warning_message = warning_message))
}