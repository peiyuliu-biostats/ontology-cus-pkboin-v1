module_UI_panel_data <- function(id)
{
  ns <- NS(id)
  tagList(
    fluidRow(column(10, h4("Scores", align="center"),
                    tableOutput(ns("sig_table"))))
  )
}

module_server_panel_data <- function(input, output, session, all_rv)
{
  ns <- session$ns
  
  # Data table 
  output$sig_table<-renderTable({
    all_rv$PK_data()$Score_dt
  })
  
}


