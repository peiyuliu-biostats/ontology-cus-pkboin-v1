# =========================================================
#   Module: Upload Mode - Safety Endpoint Setting (Sigmoid)
# =========================================================

module_UI_inv_upload_sigmoid_safe_endpoint <- function(id, index, init = NULL) {
  ns <- NS(id)
  # initial weight comes from the store, so a re-inserted block keeps its value
  v_w <- if (is.null(init)) 1 else init$weight
  tagList(
    h6(paste0("Settings for Safety Endpoint ", index, ":")),
    fluidRow(
      column(
        width = 5,
        sliderInput(
          ns("safe_weight"), label = 'Weight',
          value = v_w, min = 0, max = 10, step = 0.5, width = "100%"
        )
      ),
      column(width = 7,
             fluidRow(
               column(width = 6, uiOutput(ns("safe_slope_est_ui"))),
               column(width = 6, uiOutput(ns("safe_intercept_est_ui")))
             )
      )
    ),
    hr()
  )
}


module_server_inv_upload_sigmoid_safe_endpoint <- function(input, output, session, index, all_rv) {
  ns <- session$ns

  # estimates read the store directly, so a re-inserted block shows them right away
  output$safe_slope_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Slope = <br>", round(all_rv$safe_endpoint_setting$safe_slope[index], 2))))
  })
  output$safe_intercept_est_ui <- renderUI({
    h6(HTML(paste0("Estimated Intercept = <br>", round(all_rv$safe_endpoint_setting$safe_intercept[index], 2))))
  })

  observeEvent(debounce(reactive(input$safe_weight), 300)(), {
    all_rv$safe_endpoint_setting$safe_weight[index] <- isolate(input$safe_weight)
  })
}
