module_UI_utility_stepwise_upload_noInv <- function(id)
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
                tags$code("endpoint"), ",", tags$code("measurement"), ", and ", 
                tags$code("score")),
        tags$li(tags$code("endpoint"), "is a categorical variable with two categories: ", 
                tags$code("safety"), " and ", tags$code("efficacy"))
      )),
    hr(),
    fluidRow(
      column(width = 6,
             DT::dataTableOutput(ns("eff_stepwise_table"))
      ),
      column(width = 6,
             DT::dataTableOutput(ns("safe_stepwise_table"))
      )
    )
  )
}

module_server_utility_stepwise_upload_noInv <- function(input, output, session, all_rv)
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
    update_stepwise_dt(new_data, all_rv)   
    showNotification("Stepwise utility function parameter dataset uploaded successfully!", type = "message")
  })
  
  output$eff_stepwise_table <- DT::renderDataTable({
    req(all_rv$utility_stepwise_setting$eff_knot_num)
    eff_knot_num <- all_rv$utility_stepwise_setting$eff_knot_num
    eff_dt <- data.frame(measurement = all_rv$utility_stepwise_setting$eff_measure[1:eff_knot_num],
                         score = all_rv$utility_stepwise_setting$eff_score[1:eff_knot_num])
    DT::datatable(
      eff_dt,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = "_all"),  
                       list(width = '50%', targets = c(0, 1))         
                     )),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center;",
        "Efficacy Stepwise Parameters"
      )
    )
  })
  
  # --- Render Safety Table ---
  output$safe_stepwise_table <- DT::renderDataTable({
    req(all_rv$utility_stepwise_setting$safe_knot_num)
    safe_knot_num <- all_rv$utility_stepwise_setting$safe_knot_num
    safe_dt <- data.frame(measurement = all_rv$utility_stepwise_setting$safe_measure[1:safe_knot_num],
                         score = all_rv$utility_stepwise_setting$safe_score[1:safe_knot_num])
    DT::datatable(
      safe_dt,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = "_all"),  
                       list(width = '50%', targets = c(0, 1))         
                     )),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center;",
        "Safety Stepwise Parameters"
      )
    )
  })
}