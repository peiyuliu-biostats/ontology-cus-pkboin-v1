module_UI_inv_upload_sigmoid_eff_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial weight comes from the store, so a re-inserted block keeps its value
  v_w <- if (is.null(init)) 1 else init$weight
  tagList(
    h6(paste0("Settings for Efficacy Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(ns("eff_weight"), label = 'Weight',
                    value = v_w, min = 0, max = 10, step = 0.5, width = "100%")
      ),
      column(width = 7,
             fluidRow(
               column(width = 6, uiOutput(ns("eff_slope_est_ui"))),
               column(width = 6, uiOutput(ns("eff_intercept_est_ui")))
             )
      )
    ),
    hr()
  )
}

module_server_inv_upload_sigmoid_eff_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # estimates read the store directly, so a re-inserted block shows them right away
  output$eff_slope_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Slope = <br>", round(all_rv$eff_endpoint_setting$eff_slope[index], 2))))
  })
  output$eff_intercept_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Intercept = <br>", round(all_rv$eff_endpoint_setting$eff_intercept[index], 2))))
  })

  observeEvent(debounce(reactive(input$eff_weight), 300)(), {
    all_rv$eff_endpoint_setting$eff_weight[index] <- isolate(input$eff_weight)
  })
}
