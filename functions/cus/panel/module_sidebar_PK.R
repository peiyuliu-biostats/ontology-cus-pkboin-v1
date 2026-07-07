module_UI_sidebar_PK <- function(id)
{
  ns <- NS(id)

  tagList(
    h6("PK Dataset Setting"),
    fluidRow(
      column(
        width = 6,
        numericInput(ns("PKmin"), label = 'min', value = 1, min = 0, max = 10000, step = 1),
        uiOutput(ns("PKmin_warning"))
      ),
      column(
        width = 6,
        numericInput(ns("PKmax"), label = 'max', value = 2, min = 0, max = 10000, step = 1),
        uiOutput(ns("PKmax_warning"))
      )
    )
  )
}

module_server_sidebar_PK <- function(input, output, session, all_rv)
{
  ns <- session$ns

  # writeback only on external data update, never on user typing
  observeEvent(all_rv$triggers$update_ER_dataset, {
    updateNumericInput(session, "PKmin", value = all_rv$PK_setting$PK_min)
    updateNumericInput(session, "PKmax", value = all_rv$PK_setting$PK_max)
  }, ignoreInit = TRUE)

  # update the PK parameters
  observeEvent(debounce(reactive(input$PKmin), 300)(), {
    new_num <- isolate(as.numeric(input$PKmin))
    warning <-
      fun_numeric_check_warning(input_value = new_num,
                                lower_bound = 0, upper_bound = all_rv$PK_setting$PK_max,
                                check_int = F)
    if (warning$show_warning == TRUE) {
      showFeedbackWarning(inputId = ns("PKmin"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("PKmin"))
      all_rv$PK_setting$PK_min <- new_num
    }
  })

  observeEvent(debounce(reactive(input$PKmax), 300)(), {
    new_num <- isolate(as.numeric(input$PKmax))
    warning <-
      fun_numeric_check_warning(input_value = new_num,
                                lower_bound = all_rv$PK_setting$PK_min, upper_bound = 10000,
                                check_int = F)
    if (warning$show_warning == TRUE) {
      showFeedbackWarning(inputId = ns("PKmax"), text = warning$warning_message)
      # keep last valid value: do not write NA
    } else {
      hideFeedback(ns("PKmax"))
      all_rv$PK_setting$PK_max <- new_num
    }
  })
}
