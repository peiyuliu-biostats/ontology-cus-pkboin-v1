module_UI_inv_upload_Emax_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial weight comes from the store, so a re-inserted block keeps its value
  v_w <- if (is.null(init)) 1 else init$weight
  tagList(
    h6(paste0("Settings for Safety Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(ns("safe_weight"), label = "Weight",
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(
        width = 7,
        fluidRow(
          column(width = 6, uiOutput(ns("safe_baseline_est_ui"))),
          column(width = 6, uiOutput(ns("safe_Emax_est_ui")))
        ),
        fluidRow(
          column(width = 6, uiOutput(ns("safe_EC50_est_ui"))),
          column(width = 6, uiOutput(ns("safe_hill_est_ui")))
        )
      )
    ),
    hr()
  )
}

module_server_inv_upload_Emax_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # estimates read the store directly, so a re-inserted block shows them right away
  output$safe_baseline_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Baseline = <br>", round(all_rv$safe_endpoint_setting$safe_baseline[index], 2))))
  })
  output$safe_Emax_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Emax = <br>", round(all_rv$safe_endpoint_setting$safe_Emax[index], 2))))
  })
  output$safe_EC50_est_ui <- renderUI({
    h6(HTML(paste0("Estimated EC50 = <br>", round(all_rv$safe_endpoint_setting$safe_EC50[index], 2))))
  })
  output$safe_hill_est_ui <- renderUI({
    h6(HTML("Fix Hill as <br> 1"))
  })

  observeEvent(debounce(reactive(input$safe_weight), 300)(), {
    all_rv$safe_endpoint_setting$safe_weight[index] <- isolate(input$safe_weight)
  })
}
