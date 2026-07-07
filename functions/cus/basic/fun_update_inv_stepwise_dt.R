update_inv_stepwise_dt <- function(new_data, all_rv)
{
  eff_num <- all_rv$endpoint_num_setting$eff_num 
  safe_num <- all_rv$endpoint_num_setting$safe_num 
  
  if(eff_num > 0) {
    for(i in 1:eff_num) {
      inv_eff_dt <- new_data %>% filter(endpoint == paste0("efficacy_V", i)) %>% 
        arrange(measurement)
      inv_eff_knot_num <- nrow(inv_eff_dt)
      all_rv$individual_utility_stepwise_setting$eff_knot_num[i] <- inv_eff_knot_num
      if(inv_eff_knot_num > 0) {
        all_rv$individual_utility_stepwise_setting$eff_measure[[i]][1:inv_eff_knot_num] <- inv_eff_dt$measurement
        all_rv$individual_utility_stepwise_setting$eff_score[[i]][1:inv_eff_knot_num] <- inv_eff_dt$score
      }
    }
  }

  if (safe_num > 0) {
    for (i in 1:safe_num) {
      inv_safe_dt <- new_data %>%
        filter(endpoint == paste0("safety_V", i)) %>%
        arrange(measurement)
      
      inv_safe_knot_num <- nrow(inv_safe_dt)
      all_rv$individual_utility_stepwise_setting$safe_knot_num[i] <- inv_safe_knot_num
      
      if (inv_safe_knot_num > 0) {
        all_rv$individual_utility_stepwise_setting$safe_measure[[i]][1:inv_safe_knot_num] <- inv_safe_dt$measurement
        all_rv$individual_utility_stepwise_setting$safe_score[[i]][1:inv_safe_knot_num]  <- inv_safe_dt$score
      }
    }
  }
  

}