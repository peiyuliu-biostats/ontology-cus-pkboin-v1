# R/ui_outputs.R
# uimod 3: output results

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

uimod_content_outputs <- function() {
  fluidRow(
    # --- main result tabs ---
    tabBox(
      title = "Analysis Results", width = 12, side = "right",
      selected = "CUS Dashboard",
      
      # tab 1: main dashboard
      tabPanel("CUS Dashboard", icon = icon("chart-line"),
               fluidRow(
                 column(8, plotlyOutput("cus_plot", height = "500px")),
                 column(4, 
                        h4("Key Metrics"),
                        tableOutput("optimal_summary"),
                        p(class = "text-muted", "The optimal exposure is determined by the peak of the Clinical Utility Score curve.")
                 )
               )
      ),
      
      # tab 2: component breakdown
      tabPanel("Component Analysis", icon = icon("layer-group"),
               plotlyOutput("component_plot", height = "500px"),
               p("This plot visualizes the probability curves (or utility scores) for each individual endpoint across the exposure range.")
      ),
      
      # tab 3: weight sensitivity
      tabPanel("Weight Sensitivity", icon = icon("sliders-h"),
               fluidRow(
                 column(3, uiOutput("weight_sliders_ui")),
                 column(9, plotlyOutput("sensitivity_plot", height = "500px"))
               )
      ),
      
      # tab 4: data table
      tabPanel("Data Export", icon = icon("table"),
               downloadButton("download_report", "Download Report (HTML)"),
               br(), br(),
               DTOutput("result_table")
      )
    )
  )
}
