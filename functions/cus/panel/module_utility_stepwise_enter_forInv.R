module_UI_utility_stepwise_enter_forInv <- function(id)
{
  ns <- NS(id)

  tagList(
    # fixed anchors; per-endpoint blocks are inserted into / removed from here
    tags$div(id = ns("eff_anchor")),
    tags$div(id = ns("safe_anchor"))
  )
}

module_server_utility_stepwise_enter_forInv <- function(input, output, session, all_rv)
{
  ns <- session$ns

  # remembers rendered block count and ids that already have a server
  state <- reactiveValues(eff_n = 0, safe_n = 0,
                          eff_done = character(0), safe_done = character(0))

  # this panel only shows when stepwise + individual + enter are all selected
  is_on <- function() {
    all_rv$overall_setting$utility_type == 2 &
      all_rv$overall_setting$individual_stepwise_utility == 1 &
      all_rv$individual_utility_stepwise_setting$upload_or_not == 2
  }

  # add / remove one efficacy endpoint block; start server only the first time
  add_eff <- function(i) {
    mod_id <- paste0("eff_utility_stepwise_", i)
    insertUI(
      selector = paste0("#", ns("eff_anchor")), where = "beforeEnd",
      ui = tags$div(id = ns(paste0("eff_block_", i)),
                    module_UI_inv_utility_stepwise_enter_eff(ns(mod_id), i))
    )
    if (!(mod_id %in% state$eff_done)) {
      callModule(module_server_inv_utility_stepwise_enter_eff, mod_id, i, all_rv)
      state$eff_done <- c(state$eff_done, mod_id)
    }
  }
  remove_eff <- function(i) removeUI(selector = paste0("#", ns(paste0("eff_block_", i))))

  add_safe <- function(i) {
    mod_id <- paste0("safe_utility_stepwise_", i)
    insertUI(
      selector = paste0("#", ns("safe_anchor")), where = "beforeEnd",
      ui = tags$div(id = ns(paste0("safe_block_", i)),
                    module_UI_inv_utility_stepwise_enter_safe(ns(mod_id), i))
    )
    if (!(mod_id %in% state$safe_done)) {
      callModule(module_server_inv_utility_stepwise_enter_safe, mod_id, i, all_rv)
      state$safe_done <- c(state$safe_done, mod_id)
    }
  }
  remove_safe <- function(i) removeUI(selector = paste0("#", ns(paste0("safe_block_", i))))

  # efficacy: sync rendered blocks to endpoint count (clear all when panel is off)
  observe({
    eff_num <- all_rv$endpoint_num_setting$eff_num
    all_rv$overall_setting$utility_type
    all_rv$overall_setting$individual_stepwise_utility
    all_rv$individual_utility_stepwise_setting$upload_or_not
    target <- if (is_on() && !is.null(eff_num) && !is.na(eff_num)) eff_num else 0
    if (target > state$eff_n) lapply(seq.int(state$eff_n + 1, target), add_eff)
    else if (target < state$eff_n) lapply(seq.int(target + 1, state$eff_n), remove_eff)
    state$eff_n <- target
  })

  # safety: same logic
  observe({
    safe_num <- all_rv$endpoint_num_setting$safe_num
    all_rv$overall_setting$utility_type
    all_rv$overall_setting$individual_stepwise_utility
    all_rv$individual_utility_stepwise_setting$upload_or_not
    target <- if (is_on() && !is.null(safe_num) && !is.na(safe_num)) safe_num else 0
    if (target > state$safe_n) lapply(seq.int(state$safe_n + 1, target), add_safe)
    else if (target < state$safe_n) lapply(seq.int(target + 1, state$safe_n), remove_safe)
    state$safe_n <- target
  })
}
