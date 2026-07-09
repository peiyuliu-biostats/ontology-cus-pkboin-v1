# overall efficacy endpoint setting
module_UI_efficacy_endpoint_setting <- function(id)
{
  ns <- NS(id)
  tagList(
    h5("Efficacy Endpoint Setting:"),
    # fixed anchor; endpoint blocks are inserted into / removed from here
    tags$div(id = ns("eff_endpoint_anchor"))
  )
}


module_server_efficacy_endpoint_setting <- function(input, output, session, all_rv)
{
  ns <- session$ns

  # remembers how many blocks are currently rendered and under which structure,
  # only add/remove the difference instead of rebuilding everything.
  # called_ids keeps module ids already started, so re-inserting a block does not
  # start its server twice (removeUI clears the dom but not the observers)
  state <- reactiveValues(rendered_n = 0, structure_key = NULL, called_ids = character(0))

  # resolves endpoint i's model from its type + the group's shared regression (both modes).
  # type_vec is set by the user (simulate) or by the data (upload). falls back to scalar.
  eff_simu_model <- function(i) {
    tv <- all_rv$overall_setting$eff_type_vec
    if (!is.null(tv) && length(tv) >= i && !is.na(tv[i])) {
      if (tv[i] == "cont") all_rv$overall_setting$eff_cont_model
      else all_rv$overall_setting$eff_bin_model
    } else all_rv$overall_setting$eff_PK_model
  }

  # picks the per-endpoint UI / server builder. the block kind depends on endpoint i's own
  # resolved model (Emax has its own inputs; sigmoid + continuous share slope/intercept).
  get_builder_i <- function(i) {
    simu  <- all_rv$overall_setting$simu_or_not
    if (simu == 1) {
      model <- eff_simu_model(i)
      if (model == 2) {
        list(ui = module_UI_inv_simu_Emax_eff_endpoint,
             server = module_server_inv_simu_Emax_eff_endpoint, kind = "E")
      } else {   # sigmoid (1) or continuous (3,4,5): same slope/intercept inputs
        list(ui = module_UI_inv_simu_sigmoid_eff_endpoint,
             server = module_server_inv_simu_sigmoid_eff_endpoint, kind = "S")
      }
    } else {
      model <- eff_simu_model(i)   # upload: resolved per endpoint from detected type
      if (model == 1) {
        list(ui = module_UI_inv_upload_sigmoid_eff_endpoint,
             server = module_server_inv_upload_sigmoid_eff_endpoint, kind = "uS")
      } else if (model == 2) {
        list(ui = module_UI_inv_upload_Emax_eff_endpoint,
             server = module_server_inv_upload_Emax_eff_endpoint, kind = "uE")
      } else {
        list(ui = module_UI_inv_upload_continuous_eff_endpoint,
             server = module_server_inv_upload_continuous_eff_endpoint, kind = "uC")
      }
    }
  }

  # reads current stored values for endpoint i, used as the block's initial values
  # so an inserted / re-inserted block shows the current value, not a hard-coded default
  eff_init <- function(i) {
    s <- all_rv$eff_endpoint_setting
    tv <- all_rv$overall_setting$eff_type_vec
    is_cont <- !is.null(tv) && length(tv) >= i && !is.na(tv[i]) && tv[i] == "cont"
    list(weight = s$eff_weight[i], slope = s$eff_slope[i], intercept = s$eff_intercept[i],
         baseline = s$eff_baseline[i], Emax = s$eff_Emax[i], EC50 = s$eff_EC50[i], hill = s$eff_hill[i],
         resp_lb = s$eff_resp_lb[i], resp_ub = s$eff_resp_ub[i], is_cont = is_cont)
  }

  # inserts one endpoint block at index i; starts its server only the first time.
  # the block kind is baked into the module id so a different kind at the same index
  # gets its own server instance.
  add_block <- function(i) {
    b <- get_builder_i(i)
    mod_id <- paste0("eff_endpoint_", b$kind, "_", i)
    insertUI(
      selector = paste0("#", ns("eff_endpoint_anchor")),
      where = "beforeEnd",
      ui = tags$div(
        id = ns(paste0("eff_block_", i)),  # wrapper id used by removeUI
        b$ui(ns(mod_id), i, eff_init(i))
      )
    )
    if (!(mod_id %in% state$called_ids)) {
      callModule(b$server, mod_id, i, all_rv)
      state$called_ids <- c(state$called_ids, mod_id)
    }
  }

  # removes the block at index i from the page
  remove_block <- function(i) {
    removeUI(selector = paste0("#", ns(paste0("eff_block_", i))))
  }

  # structure key = mode + per-endpoint block kinds. changes whenever the mode, the count,
  # or any endpoint's block kind changes, triggering a clean rebuild.
  eff_structure_key <- function(n) {
    if (is.na(n) || n <= 0) return(paste0(all_rv$overall_setting$simu_or_not, ":empty"))
    kinds <- vapply(seq_len(n), function(i) get_builder_i(i)$kind, character(1))
    paste0(all_rv$overall_setting$simu_or_not, ":", paste(kinds, collapse = ""))
  }

  observe({
    # depend on count and on mode/model/type so blocks update when any of them changes
    eff_num <- all_rv$endpoint_num_setting$eff_num
    all_rv$overall_setting$simu_or_not
    all_rv$overall_setting$eff_PK_model
    all_rv$overall_setting$eff_type_vec
    all_rv$overall_setting$eff_cont_model
    all_rv$overall_setting$eff_bin_model
    req(!is.null(eff_num), !is.na(eff_num), eff_num >= 0)

    key <- eff_structure_key(eff_num)

    # structure changed: clear all blocks then rebuild from scratch
    if (is.null(state$structure_key) || state$structure_key != key) {
      if (state$rendered_n > 0)
        lapply(seq_len(state$rendered_n), remove_block)
      lapply(seq_len(eff_num), add_block)
      state$structure_key <- key
      state$rendered_n <- eff_num
      return()
    }

    # only the count changed: add or remove just the difference, leave the rest intact
    if (eff_num > state$rendered_n) {
      lapply(seq.int(state$rendered_n + 1, eff_num), add_block)
    } else if (eff_num < state$rendered_n) {
      lapply(seq.int(eff_num + 1, state$rendered_n), remove_block)
    }
    state$rendered_n <- eff_num
  })
}
