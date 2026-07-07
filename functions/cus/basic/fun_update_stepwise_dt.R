update_stepwise_dt <- function(new_data, all_rv)
{
  safe_dt <- new_data %>% filter(endpoint == "safety") %>% arrange(measurement)
  eff_dt <- new_data %>% filter(endpoint == "efficacy") %>% arrange(measurement)
  
  safe_knot_num <- nrow(safe_dt)
  eff_knot_num <- nrow(eff_dt)
  
  all_rv$utility_stepwise_setting$safe_knot_num <- safe_knot_num
  all_rv$utility_stepwise_setting$eff_knot_num <- eff_knot_num
  
  if(safe_knot_num > 0) {
    all_rv$utility_stepwise_setting$safe_measure <- safe_dt$measurement
    all_rv$utility_stepwise_setting$safe_score <- safe_dt$score
  }
  
  if(eff_knot_num > 0) {
    all_rv$utility_stepwise_setting$eff_measure <- eff_dt$measurement
    all_rv$utility_stepwise_setting$eff_score <- eff_dt$score
  }
}