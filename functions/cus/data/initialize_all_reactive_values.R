initialize_all_reactive_values <- function() {
  overall_setting <- initial_overall_setting()
  
  # trigger 
  triggers <- initial_triggers()
  
  # Data #### 
  ER_data_list <- initial_ER_data_list()
  
  # Specific Settings ####
  PK_setting <- initial_PK_setting()  
  endpoint_num_setting <- initial_endpoint_num_setting()
  eff_endpoint_setting <- initial_eff_endpoint_setting()
  safe_endpoint_setting <- initial_safe_endpoint_setting()
  utility_Sshape_setting <- initial_utility_Sshape_setting()
  utility_stepwise_setting <- initial_utility_stepwise_setting()
  
  # individual specific settings #### 
  inidividual_utility_Sshape_setting <- initial_inidividual_utility_Sshape_setting()
  individual_utility_stepwise_setting <- initial_individual_utility_stepwise_setting()
  # Generate simulated PK data
 
  PK_data <- reactive({
    # simulate PK data
    initial_PK_data(PK_setting, endpoint_num_setting, eff_endpoint_setting, safe_endpoint_setting, 
                    utility_Sshape_setting, inidividual_utility_Sshape_setting,
                    utility_stepwise_setting, individual_utility_stepwise_setting,
                    overall_setting, ER_data_list)

  })
                              
  # Save all values into all_rv 
  all_initial_values <- 
    list(overall_setting = overall_setting,
         triggers = triggers,
         ER_data_list = ER_data_list, 
         PK_setting = PK_setting,
         endpoint_num_setting = endpoint_num_setting,
         eff_endpoint_setting = eff_endpoint_setting, 
         safe_endpoint_setting = safe_endpoint_setting,
         utility_Sshape_setting = utility_Sshape_setting,
         utility_stepwise_setting = utility_stepwise_setting,
         inidividual_utility_Sshape_setting = inidividual_utility_Sshape_setting,
         individual_utility_stepwise_setting = individual_utility_stepwise_setting,
         PK_data = PK_data)
  
  return(all_initial_values)
}
