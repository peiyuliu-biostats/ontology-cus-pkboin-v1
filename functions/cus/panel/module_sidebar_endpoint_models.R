# =========================================================
#   Sidebar sub-module: Endpoint Type & Regression (B scheme, simulate mode)
#   left column  : per-endpoint type radio (continuous / binary), one row per endpoint
#   right column : one shared regression per type actually used in the group
#                  (continuous box shown only if any cont endpoint; binary box only if any bin)
#   writes: overall_setting$eff_type_vec[i] / safe_type_vec[i] (per endpoint),
#           overall_setting$eff_cont_model / eff_bin_model / safe_* (shared scalars)
# =========================================================

module_UI_sidebar_endpoint_models <- function(id) {
  ns <- NS(id)
  tagList(
    # Q1: endpoint types (efficacy | safety)
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Endpoint Types"),
    uiOutput(ns("types_block")),
    # Q2: regression models (efficacy | safety), shown per type actually used
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Regression Models"),
    uiOutput(ns("models_block"))
  )
}

# helper: builds ONLY the per-endpoint type radios for one group (efficacy or safety).
.build_group_types_ui <- function(ns, prefix, group_label, num, types) {
  if (is.na(num) || num <= 0) return(NULL)
  upper <- toupper(prefix)
  type_rows <- lapply(seq_len(num), function(i) {
    sel <- if (length(types) >= i && !is.na(types[i])) types[i] else "bin"
    div(style = "margin-bottom:4px;",
        radioButtons(ns(paste0(prefix, "_type_", i)),
                     label = paste0(upper, i),
                     choices = list("cont" = "cont", "bin" = "bin"),
                     selected = sel, inline = TRUE))
  })
  tagList(
    div(class = "custom-label", group_label),
    type_rows
  )
}

# helper: builds ONLY the shared per-type regression radios for one group.
# continuous box shows only if the group has any continuous endpoint; binary box
# only if any binary endpoint (driven by the *_has_cont / *_has_bin flags).
.build_group_models_ui <- function(ns, prefix, group_label, num, cont_m, bin_m) {
  if (is.na(num) || num <= 0) return(NULL)
  cont_choices <- list("Linear" = 3, "Log-linear" = 4, "Exponential" = 5)
  bin_choices  <- list("Logistic" = 1, "Emax" = 2)
  tagList(
    div(class = "custom-label", group_label),
    conditionalPanel(
      condition = paste0("output['", ns(paste0(prefix, "_has_cont")), "'] == true"),
      radioButtons(ns(paste0(prefix, "_cont_model")),
                   label = span("continuous ",
                                tags$span(`data-toggle` = "tooltip",
                                          title = "Log = Log-linear regression",
                                          style = "cursor:help; color:#be2bbb;", "(?)")),
                   choices = cont_choices,
                   selected = if (cont_m %in% c(3, 4, 5)) cont_m else 3, inline = TRUE)
    ),
    conditionalPanel(
      condition = paste0("output['", ns(paste0(prefix, "_has_bin")), "'] == true"),
      radioButtons(ns(paste0(prefix, "_bin_model")),
                   label = "binary",
                   choices = bin_choices,
                   selected = if (bin_m %in% c(1, 2)) bin_m else 1, inline = TRUE)
    ),
    tags$script(HTML(sprintf("$(function(){ $('#%s [data-toggle=\"tooltip\"]').tooltip(); });", ns(prefix))))
  )
}


module_server_sidebar_endpoint_models <- function(input, output, session, all_rv) {
  ns <- session$ns

  # Q1: types section, efficacy column | safety column. rebuild when counts change.
  output$types_block <- renderUI({
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    fluidRow(
      column(width = 6,
             .build_group_types_ui(ns, "eff", "Efficacy", eff_num,
                                   isolate(all_rv$overall_setting$eff_type_vec))),
      column(width = 6,
             .build_group_types_ui(ns, "safe", "Safety", safe_num,
                                   isolate(all_rv$overall_setting$safe_type_vec)))
    )
  })

  # Q2: regression section, efficacy column | safety column. rebuild when counts change.
  output$models_block <- renderUI({
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    fluidRow(
      column(width = 6,
             .build_group_models_ui(ns, "eff", "Efficacy", eff_num,
                                    isolate(all_rv$overall_setting$eff_cont_model),
                                    isolate(all_rv$overall_setting$eff_bin_model))),
      column(width = 6,
             .build_group_models_ui(ns, "safe", "Safety", safe_num,
                                    isolate(all_rv$overall_setting$safe_cont_model),
                                    isolate(all_rv$overall_setting$safe_bin_model)))
    )
  })

  # flags that drive the conditionalPanels (which shared regression boxes to show)
  output$eff_has_cont  <- reactive(any(all_rv$overall_setting$eff_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$eff_num, 0))] == "cont", na.rm = TRUE))
  output$eff_has_bin   <- reactive(any(all_rv$overall_setting$eff_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$eff_num, 0))] == "bin",  na.rm = TRUE))
  output$safe_has_cont <- reactive(any(all_rv$overall_setting$safe_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$safe_num, 0))] == "cont", na.rm = TRUE))
  output$safe_has_bin  <- reactive(any(all_rv$overall_setting$safe_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$safe_num, 0))] == "bin",  na.rm = TRUE))
  outputOptions(output, "eff_has_cont",  suspendWhenHidden = FALSE)
  outputOptions(output, "eff_has_bin",   suspendWhenHidden = FALSE)
  outputOptions(output, "safe_has_cont", suspendWhenHidden = FALSE)
  outputOptions(output, "safe_has_bin",  suspendWhenHidden = FALSE)

  # observers that write per-endpoint types into the store. one observer per slot (max 10),
  # guarded by req() so only the currently-rendered radios write. avoids re-registering
  # observers on every count change (which would accumulate).
  lapply(seq_len(10), function(i) {
    observeEvent(input[[paste0("eff_type_", i)]], {
      all_rv$overall_setting$eff_type_vec[i] <- input[[paste0("eff_type_", i)]]
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("safe_type_", i)]], {
      all_rv$overall_setting$safe_type_vec[i] <- input[[paste0("safe_type_", i)]]
    }, ignoreInit = TRUE)
  })

  # shared per-type regression writebacks
  observeEvent(input$eff_cont_model,  { all_rv$overall_setting$eff_cont_model  <- as.numeric(input$eff_cont_model) })
  observeEvent(input$eff_bin_model,   { all_rv$overall_setting$eff_bin_model   <- as.numeric(input$eff_bin_model) })
  observeEvent(input$safe_cont_model, { all_rv$overall_setting$safe_cont_model <- as.numeric(input$safe_cont_model) })
  observeEvent(input$safe_bin_model,  { all_rv$overall_setting$safe_bin_model  <- as.numeric(input$safe_bin_model) })
}


# =========================================================
#   UPLOAD variant: endpoint type is DETECTED from the data (read-only); the user only
#   picks the shared per-type regression. continuous box shows only if the group has any
#   continuous endpoint, binary box only if any binary endpoint.
# =========================================================

module_UI_sidebar_endpoint_models_upload <- function(id) {
  ns <- NS(id)
  tagList(
    # Q1: endpoint types (detected, read-only), efficacy | safety
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Endpoint Types"),
    div(style = "font-size:12px; color:#888;", "types are detected from the uploaded data"),
    uiOutput(ns("types_block")),
    # Q2: regression models (efficacy | safety), shown per type actually used
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Regression Models"),
    uiOutput(ns("models_block"))
  )
}

# helper: builds ONLY the detected read-only type rows for one uploaded group.
.build_group_types_ui_upload <- function(ns, prefix, group_label, num, types) {
  if (is.na(num) || num <= 0) return(NULL)
  upper <- toupper(prefix)
  type_rows <- lapply(seq_len(num), function(i) {
    tp <- if (length(types) >= i && !is.na(types[i]) && types[i] == "cont") "continuous" else "binary"
    div(style = "margin-bottom:4px; font-size:13px;",
        tags$b(paste0(upper, i)), tags$span(style = "color:#555;", paste0(": ", tp)))
  })
  tagList(
    div(class = "custom-label", group_label),
    type_rows
  )
}

# helper: builds ONLY the shared per-type regression radios for one uploaded group.
.build_group_models_ui_upload <- function(ns, prefix, group_label, num, cont_m, bin_m) {
  if (is.na(num) || num <= 0) return(NULL)
  cont_choices <- list("Linear" = 3, "Log-linear" = 4, "Exponential" = 5)
  bin_choices  <- list("Logistic" = 1, "Emax" = 2)
  tagList(
    div(class = "custom-label", group_label),
    conditionalPanel(
      condition = paste0("output['", ns(paste0(prefix, "_has_cont")), "'] == true"),
      radioButtons(ns(paste0(prefix, "_cont_model")),
                   label = span("continuous ",
                                tags$span(`data-toggle` = "tooltip",
                                          title = "Log = Log-linear regression",
                                          style = "cursor:help; color:#be2bbb;", "(?)")),
                   choices = cont_choices,
                   selected = if (cont_m %in% c(3, 4, 5)) cont_m else 3, inline = TRUE)
    ),
    conditionalPanel(
      condition = paste0("output['", ns(paste0(prefix, "_has_bin")), "'] == true"),
      radioButtons(ns(paste0(prefix, "_bin_model")),
                   label = "binary",
                   choices = bin_choices,
                   selected = if (bin_m %in% c(1, 2)) bin_m else 1, inline = TRUE)
    ),
    tags$script(HTML(sprintf("$(function(){ $('#%s [data-toggle=\"tooltip\"]').tooltip(); });", ns(prefix))))
  )
}

module_server_sidebar_endpoint_models_upload <- function(input, output, session, all_rv) {
  ns <- session$ns

  # Q1: detected types, efficacy column | safety column. rebuild on new upload.
  output$types_block <- renderUI({
    all_rv$triggers$update_ER_dataset
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    fluidRow(
      column(width = 6,
             .build_group_types_ui_upload(ns, "eff", "Efficacy", eff_num,
                                          isolate(all_rv$overall_setting$eff_type_vec))),
      column(width = 6,
             .build_group_types_ui_upload(ns, "safe", "Safety", safe_num,
                                          isolate(all_rv$overall_setting$safe_type_vec)))
    )
  })

  # Q2: regression models, efficacy column | safety column. rebuild on new upload.
  output$models_block <- renderUI({
    all_rv$triggers$update_ER_dataset
    eff_num  <- all_rv$endpoint_num_setting$eff_num
    safe_num <- all_rv$endpoint_num_setting$safe_num
    fluidRow(
      column(width = 6,
             .build_group_models_ui_upload(ns, "eff", "Efficacy", eff_num,
                                           isolate(all_rv$overall_setting$eff_cont_model),
                                           isolate(all_rv$overall_setting$eff_bin_model))),
      column(width = 6,
             .build_group_models_ui_upload(ns, "safe", "Safety", safe_num,
                                           isolate(all_rv$overall_setting$safe_cont_model),
                                           isolate(all_rv$overall_setting$safe_bin_model)))
    )
  })

  output$eff_has_cont  <- reactive(any(all_rv$overall_setting$eff_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$eff_num, 0))] == "cont", na.rm = TRUE))
  output$eff_has_bin   <- reactive(any(all_rv$overall_setting$eff_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$eff_num, 0))] == "bin",  na.rm = TRUE))
  output$safe_has_cont <- reactive(any(all_rv$overall_setting$safe_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$safe_num, 0))] == "cont", na.rm = TRUE))
  output$safe_has_bin  <- reactive(any(all_rv$overall_setting$safe_type_vec[seq_len(coalesce(all_rv$endpoint_num_setting$safe_num, 0))] == "bin",  na.rm = TRUE))
  outputOptions(output, "eff_has_cont",  suspendWhenHidden = FALSE)
  outputOptions(output, "eff_has_bin",   suspendWhenHidden = FALSE)
  outputOptions(output, "safe_has_cont", suspendWhenHidden = FALSE)
  outputOptions(output, "safe_has_bin",  suspendWhenHidden = FALSE)

  # regression writebacks (type is read-only here, so no type observers)
  observeEvent(input$eff_cont_model,  { all_rv$overall_setting$eff_cont_model  <- as.numeric(input$eff_cont_model) })
  observeEvent(input$eff_bin_model,   { all_rv$overall_setting$eff_bin_model   <- as.numeric(input$eff_bin_model) })
  observeEvent(input$safe_cont_model, { all_rv$overall_setting$safe_cont_model <- as.numeric(input$safe_cont_model) })
  observeEvent(input$safe_bin_model,  { all_rv$overall_setting$safe_bin_model  <- as.numeric(input$safe_bin_model) })
}
