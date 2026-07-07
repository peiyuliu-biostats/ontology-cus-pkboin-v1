module_UI_panel_endpoint <- function(id)
{
  ns <- NS(id)
  tagList(
    module_UI_efficacy_endpoint_setting(ns("efficacy_endpoint_setting")),
    module_UI_safety_endpoint_setting(ns("safety_endpoint_setting")),
    module_UI_PK_endpoint_simu_fig(ns("PK_endpoint_simu_fig")),
    module_UI_PK_endpoint_upload_fig(ns("PK_endpoint_upload_fig"))
  )
  
}

module_server_panel_endpoint <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  ### Efficacy Endpoint 
  callModule(module = module_server_efficacy_endpoint_setting, id = "efficacy_endpoint_setting",
             all_rv)
  ### Safety Endpoint 
  callModule(module = module_server_safety_endpoint_setting, id = "safety_endpoint_setting",
             all_rv)
  
  ### PK-Endpoint Regression Curve for Simulated Data 
  callModule(module = module_server_PK_endpoint_simu_fig, id = "PK_endpoint_simu_fig", all_rv)
  
  ### PK-Endpoint Regression Curve for Uploaded Data 
  callModule(module = module_server_PK_endpoint_upload_fig, id = "PK_endpoint_upload_fig", all_rv)
  
  
}