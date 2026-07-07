module_UI_utility_stepwise_upload_forInv <- function(id)
{
  ns <- NS(id)
  tagList(
    # upload parameter dataset 
    fileInput(ns("stepwise_upload_param"), 
              label = tags$span(
                "Upload stepwise utility function parameter dataset:",
                class = "custom-label",
                style = "display:inline-block; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; max-width: 100%;"
              ),
              accept = c(".csv", ".xlsx", "text/csv", "text/comma-separated-values", "text/plain", ".txt", ".rds")),
    helpText(
      "Data Requirements: ",
      tags$ol(
        tags$li("Acceptable formats: .csv, .xlsx, .txt, .rds;"), 
        tags$li("The dataset should include three columns:", 
                tags$code("endpoint"), ", ", tags$code("measurement"), ", and ", 
                tags$code("score")),
        tags$li(tags$code("endpoint"), "is a categorical variable listing the endpoint names, following the format ", 
                tags$code("safety_V1"), ", ", tags$code("safety_V2"), ",...,", 
                tags$code("efficacy_V1"), ", ", tags$code("efficacy_V2"), ", ... .")
      )),
    hr(),
    uiOutput(ns("UI_stepwise_upload_eff_for_inv")),
    uiOutput(ns("UI_stepwise_upload_safe_for_inv"))
  )
}

module_server_utility_stepwise_upload_forInv <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  
  # upload the stepwise parameter dataset 
  observeEvent(input$stepwise_upload_param, {
    req(input$stepwise_upload_param)                           
    req(all_rv$utility_stepwise_setting$upload_or_not == 1)       
    
    file_path <- input$stepwise_upload_param$datapath
    file_ext <- tools::file_ext(file_path)
    
    new_data <- data.frame()
    if (file_ext == "csv" || file_ext == "txt") {
      new_data <- read.csv(file_path, header = TRUE)
    } else if (file_ext == "xlsx") {
      new_data <- readxl::read_excel(file_path)
    } else if (file_ext == "rds") {
      new_data <- readRDS(file_path)
    } else {
      showNotification("Unsupported file type.", type = "error")
      return(NULL)
    }
    # validate the file matches separate-per-endpoint format before writing.
    # this mode reads endpoints by name efficacy_V1 / safety_V1 / ...; a file that
    # only has "efficacy" / "safety" (shared-utility format) would match nothing
    # and silently leave the defaults in place, misleading the user.
    if (is.null(new_data) || !is.data.frame(new_data) || !("endpoint" %in% colnames(new_data))) {
      showNotification("Uploaded file must contain an 'endpoint' column.", type = "error", duration = 10)
      return(NULL)
    }
    ep_vals <- as.character(new_data$endpoint)
    has_versioned <- any(grepl("^(efficacy|safety)_V[0-9]+$", ep_vals, ignore.case = TRUE))
    if (!has_versioned) {
      showNotification(
        paste0("This file does not match the separate-per-endpoint format. ",
               "The 'endpoint' column must use names like efficacy_V1, safety_V1, ... ",
               "If your file has one shared curve per type (efficacy / safety), ",
               "switch 'Provide separate utility per endpoint?' to No."),
        type = "error", duration = 12
      )
      return(NULL)
    }

    update_inv_stepwise_dt(new_data, all_rv)   
    showNotification("Stepwise utility function parameter dataset uploaded successfully!", type = "message")
  })
  
  # UI: individual stepwise utility function for each efficacy endpoint 
  output$UI_stepwise_upload_eff_for_inv <- renderUI({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    req(!is.null(eff_num), !is.na(eff_num), eff_num > 0)
    if (all_rv$overall_setting$utility_type == 2 &                      # stepwise 
        all_rv$overall_setting$individual_stepwise_utility == 1 &       # individual
        all_rv$individual_utility_stepwise_setting$upload_or_not == 1   # upload 
        ) {
      lapply(1:eff_num, function(i) {
        module_UI_inv_utility_stepwise_upload_eff(ns(paste0("eff_utility_stepwise_", i)), i)
      })
    }
  })
  
  # UI: individual stepwise utility function for each safety endpoint 
  output$UI_stepwise_upload_safe_for_inv <- renderUI({
    safe_num <- all_rv$endpoint_num_setting$safe_num
    req(!is.null(safe_num), !is.na(safe_num), safe_num > 0)
    if (all_rv$overall_setting$utility_type == 2 &                      # stepwise 
        all_rv$overall_setting$individual_stepwise_utility == 1 &       # individual
        all_rv$individual_utility_stepwise_setting$upload_or_not == 1   # upload 
    ) {
      lapply(1:safe_num, function(i) {
        module_UI_inv_utility_stepwise_upload_safe(ns(paste0("safe_utility_stepwise_", i)), i)
      })
    }
  })
  
  # server: 
  observe({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    req(!is.null(eff_num), !is.na(eff_num), eff_num > 0)
    lapply(1:eff_num, function(i) {
      if (all_rv$overall_setting$utility_type == 2 &                      # stepwise 
          all_rv$overall_setting$individual_stepwise_utility == 1 &       # individual
          all_rv$individual_utility_stepwise_setting$upload_or_not == 1   # upload 
      ) {
        callModule(module_server_inv_utility_stepwise_upload_eff, paste0("eff_utility_stepwise_", i), i, all_rv)
      } 
    })
  })
  
  # server: 
  observe({
    safe_num <- all_rv$endpoint_num_setting$safe_num
    req(!is.null(safe_num), !is.na(safe_num), safe_num > 0)
    lapply(1:safe_num, function(i) {
      if (all_rv$overall_setting$utility_type == 2 &                      # stepwise 
          all_rv$overall_setting$individual_stepwise_utility == 1 &       # individual
          all_rv$individual_utility_stepwise_setting$upload_or_not == 1   # upload 
      ) {
        callModule(module_server_inv_utility_stepwise_upload_safe, paste0("safe_utility_stepwise_", i), i, all_rv)
      } 
    })
  })
  

}