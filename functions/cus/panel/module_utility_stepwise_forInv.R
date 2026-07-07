module_UI_utility_stepwise_forInv <- function(id)
{
  ns <- NS(id)
  tagList(
    # Upload or Enter parameters 
    radioButtons(inputId = ns("stepwise_upload_or_not"), width = "100%",
                 label = div(class = "custom-label", "How would you like to provide stepwise utility function parameters:"), 
                 choices = list("Upload parameter dataset" = 1, 
                                "Enter parameters manually" = 2), 
                 selected = 1),
    hr(),
    # choose to upload the dataset 
    conditionalPanel(
      condition = paste0("input['", ns("stepwise_upload_or_not"), "'] == 1"),
      module_UI_utility_stepwise_upload_forInv(ns("UI_utility_stepwise_upload_forInv"))
    ),
    
    # choose to enter the dataset
    conditionalPanel(
      condition = paste0("input['", ns("stepwise_upload_or_not"), "'] == 2"),
      module_UI_utility_stepwise_enter_forInv(ns("UI_utility_stepwise_enter_forInv"))
    )
  )
}

module_server_utility_stepwise_forInv <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  observeEvent(input$stepwise_upload_or_not, {
    all_rv$individual_utility_stepwise_setting$upload_or_not <- input$stepwise_upload_or_not
  })
  
  # For Upload
  callModule(module = module_server_utility_stepwise_upload_forInv, 
             id = "UI_utility_stepwise_upload_forInv", all_rv)
  
  # For Enter 
  callModule(module = module_server_utility_stepwise_enter_forInv, 
             id = "UI_utility_stepwise_enter_forInv", all_rv) 
}