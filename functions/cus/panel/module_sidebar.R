# sidebar of the main app
module_UI_sidebar <- function(id)
{
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("simu_upload"),
      label = div(class = "custom-label", "Simulate or upload the ER dataset?"),
      choices = list("Simulate ER dataset" = 1, "Upload ER dataset" = 2),
      selected = 1, 
      width = "100%"
    ),    
    uiOutput(ns("simu_upload_ui")),    # simu = 1, upload = 2
    # choose to simulate the ER dataset 
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 1"),
      # PK related parameters 
      module_UI_sidebar_PK(ns("sidebar_PK")),   
      # Choose the number of efficacy and safety endpoint
      module_UI_sidebar_endpoint(ns("sidebar_endpoint")),
      # per-endpoint type + shared per-type regression (mixed endpoints, B scheme)
      module_UI_sidebar_endpoint_models(ns("endpoint_models"))
    ),
    # choose to upload the ER dataset  
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 2"),
      # PK related parameters 
      module_UI_sidebar_upload_data(ns("upload_data")),
      # detected per-endpoint type (read-only) + shared per-type regression (mixed endpoints)
      module_UI_sidebar_endpoint_models_upload(ns("endpoint_models_upload"))
    ),
  )

}


module_server_sidebar <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  # Modules for simulating data
  callModule(module = module_server_sidebar_PK, id = "sidebar_PK", all_rv)
  callModule(module = module_server_sidebar_endpoint, id = "sidebar_endpoint", 
             all_rv$endpoint_num_setting)
  callModule(module = module_server_sidebar_endpoint_models, id = "endpoint_models", all_rv)
  
  # Modules for uploading data
  callModule(module = module_server_sidebar_upload_data, id = "upload_data", all_rv)
  callModule(module = module_server_sidebar_endpoint_models_upload, id = "endpoint_models_upload", all_rv)
  
  # update values 
  observeEvent(input$simu_upload, {
    all_rv$overall_setting$simu_or_not = input$simu_upload
    # switching back to simulate: clear stale uploaded data so endpoint / utility /
    # CUS / data panels recompute from simulation instead of showing old upload results.
    if (input$simu_upload == 1) {
      all_rv$ER_data_list$ER_rawdt <- data.frame()
      all_rv$triggers$update_ER_dataset <- Sys.time()
    }
  })

}