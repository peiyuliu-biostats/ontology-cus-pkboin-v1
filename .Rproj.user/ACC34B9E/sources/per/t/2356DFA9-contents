# R/ui_inputs.R
# ui module 2: input controls

library(shiny)
library(shinydashboard)
library(shinyBS)

#' helper to create numeric inputs with tooltip info
param_input_factory <- function(id, label, tooltip, value, min = NA, max = NA, step = NA) {
  tagList(
    div(style = "display: flex; align-items: center; justify-content: space-between;",
        tags$label(label, `for` = id),
        tags$i(class = "fa fa-info-circle", style = "color: #6f42c1; cursor: pointer;", 
               title = tooltip)   # change icon color to purple to match theme
    ),
    numericInput(id, label = NULL, value = value, min = min, max = max, step = step)
  )
}

uimod_content_inputs <- function() {
  tagList(
    fluidRow(
      # --- left box: dose-response parameters (purple via css) ---
      # rename to dose-response parameters; color follows .box-primary in app.R
      box(
        title = "Dose-Response Parameters", status = "primary", solidHeader = TRUE, width = 4,
        
        # case selection section
        h4("Case Study Library", style = "margin-top: 0;"),
        
        div(style = "display: flex; align-items: flex-end;",
            div(style = "flex-grow: 1; margin-right: 5px;",
                selectInput("demo_case", "Select Case:", 
                            choices = c("Custom / None" = "None", 
                                        "Loncastuximab tesirine (Lymphoma)", 
                                        "Polatuzumab vedotin", 
                                        "Tisotumab vedotin", 
                                        "Fedratinib", 
                                        "Lurbinectedin"))
            ),
            div(
              actionButton("case_info_btn", "", icon = icon("info-circle"), class = "btn-info", 
                           style = "margin-bottom: 15px; background-color: #D1C4E9; border-color: #D1C4E9; color: #333;", 
                           title = "View Case Details")
            )
        ),
        
        # Load button
        actionButton("load_demo", "Load Case Parameters", icon = icon("upload"), 
                     class = "btn-primary btn-block", 
                     style = "background-color: #D1C4E9; border-color: #B39DDB; color: #333; font-weight: bold;"),
        
        tags$hr(), 
        
        # parameter input section
        param_input_factory("pk_min", "Min Exposure (PK)", "Minimum value for the PK exposure range (X-axis)", 0, 0, 100, 0.1),
        param_input_factory("pk_max", "Max Exposure (PK)", "Maximum value for the PK exposure range (X-axis)", 2.0, 0.1, 100, 0.1),
        param_input_factory("pk_ref", "Reference PK", "Reference PK value (e.g., at recommended dose) for visualization", 0.66, 0, 100, 0.01),
        
        br(),
        sliderInput("n_endpoints", "Number of Endpoints", min = 1, max = 5, value = 2, step = 1),
        actionButton("calc_btn", "Calculate CUS", icon = icon("calculator"), 
                     class = "btn-success btn-block",
                     style = "background-color: #28a745; border-color: #28a745;")
      ),
      
      # --- right box: endpoint configuration (pale orange via css) ---
      # keep warning status; color overridden to light orange in app.R
      box(
        title = "Endpoint Configuration", status = "warning", solidHeader = TRUE, width = 8,
        uiOutput("endpoint_ui_container")
      )
    )
  )
}
