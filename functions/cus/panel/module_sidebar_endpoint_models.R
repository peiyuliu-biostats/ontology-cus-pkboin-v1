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
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Endpoint Type & Regression"),
    uiOutput(ns("eff_block")),
    uiOutput(ns("safe_block"))
  )
}

# helper: builds the two-column UI for one group (efficacy or safety).
# `prefix` is "eff" or "safe"; `num` is the endpoint count; `types`/`cont_m`/`bin_m`
# are the current stored values used as initial selections.
.build_group_models_ui <- function(ns, prefix, group_label, num, types, cont_m, bin_m) {
  if (is.na(num) || num <= 0) return(NULL)
  cont_choices <- list("Linear" = 3, "Log-linear" = 4, "Exponential" = 5)
  bin_choices  <- list("Logistic" = 1, "Emax" = 2)
  upper <- toupper(prefix)

  # left column: one type radio per endpoint
  type_rows <- lapply(seq_len(num), function(i) {
    sel <- if (length(types) >= i && !is.na(types[i])) types[i] else "bin"
    div(style = "margin-bottom:4px;",
        radioButtons(ns(paste0(prefix, "_type_", i)),
                     label = paste0(upper, i),
                     choices = list("cont" = "cont", "bin" = "bin"),
                     selected = sel, inline = TRUE))
  })

  any_cont <- any(types[seq_len(num)] == "cont", na.rm = TRUE)
  any_bin  <- any(types[seq_len(num)] == "bin",  na.rm = TRUE)

  tagList(
    div(class = "custom-label", group_label),
    fluidRow(
      column(
        width = 5,
        tags$div(style = "font-size:12px; color:#888;", "endpoint types"),
        type_rows
      ),
      column(
        width = 7,
        tags$div(style = "font-size:12px; color:#888;", "regression models"),
        # continuous shared box: only when at least one continuous endpoint
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
        # binary shared box: only when at least one binary endpoint
        conditionalPanel(
          condition = paste0("output['", ns(paste0(prefix, "_has_bin")), "'] == true"),
          radioButtons(ns(paste0(prefix, "_bin_model")),
                       label = "binary",
                       choices = bin_choices,
                       selected = if (bin_m %in% c(1, 2)) bin_m else 1, inline = TRUE)
        )
      )
    ),
    tags$script(HTML(sprintf("$(function(){ $('#%s [data-toggle=\"tooltip\"]').tooltip(); });", ns(prefix)))) 
  )
}


module_server_sidebar_endpoint_models <- function(input, output, session, all_rv) {
  ns <- session$ns

  # render efficacy two-column block, rebuilding when the count changes
  output$eff_block <- renderUI({
    num <- all_rv$endpoint_num_setting$eff_num
    .build_group_models_ui(ns, "eff", "Efficacy", num,
                           isolate(all_rv$overall_setting$eff_type_vec),
                           isolate(all_rv$overall_setting$eff_cont_model),
                           isolate(all_rv$overall_setting$eff_bin_model))
  })

  output$safe_block <- renderUI({
    num <- all_rv$endpoint_num_setting$safe_num
    .build_group_models_ui(ns, "safe", "Safety", num,
                           isolate(all_rv$overall_setting$safe_type_vec),
                           isolate(all_rv$overall_setting$safe_cont_model),
                           isolate(all_rv$overall_setting$safe_bin_model))
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
    hr(style = "margin-top:8px; margin-bottom:8px;"),
    h5("Endpoint Type & Regression"),
    div(style = "font-size:12px; color:#888;", "types are detected from the uploaded data"),
    uiOutput(ns("eff_block")),
    uiOutput(ns("safe_block"))
  )
}

# builds the read-only-type + selectable-regression UI for one uploaded group
.build_group_models_ui_upload <- function(ns, prefix, group_label, num, types, cont_m, bin_m) {
  if (is.na(num) || num <= 0) return(NULL)
  cont_choices <- list("Linear" = 3, "Log-linear" = 4, "Exponential" = 5)
  bin_choices  <- list("Logistic" = 1, "Emax" = 2)
  upper <- toupper(prefix)

  # left column: detected type per endpoint (read-only text)
  type_rows <- lapply(seq_len(num), function(i) {
    tp <- if (length(types) >= i && !is.na(types[i]) && types[i] == "cont") "continuous" else "binary"
    div(style = "margin-bottom:4px; font-size:13px;",
        tags$b(paste0(upper, i)), tags$span(style = "color:#555;", paste0(": ", tp)))
  })

  any_cont <- any(types[seq_len(num)] == "cont", na.rm = TRUE)
  any_bin  <- any(types[seq_len(num)] == "bin",  na.rm = TRUE)

  tagList(
    div(class = "custom-label", group_label),
    fluidRow(
      column(
        width = 5,
        tags$div(style = "font-size:12px; color:#888;", "endpoint types (detected)"),
        type_rows
      ),
      column(
        width = 7,
        tags$div(style = "font-size:12px; color:#888;", "regression models"),
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
        )
      )
    ),
    tags$script(HTML(sprintf("$(function(){ $('#%s [data-toggle=\"tooltip\"]').tooltip(); });", ns(prefix)))) 
  )
}

module_server_sidebar_endpoint_models_upload <- function(input, output, session, all_rv) {
  ns <- session$ns

  output$eff_block <- renderUI({
    all_rv$triggers$update_ER_dataset   # rebuild when a new dataset is uploaded
    num <- all_rv$endpoint_num_setting$eff_num
    .build_group_models_ui_upload(ns, "eff", "Efficacy", num,
                                  isolate(all_rv$overall_setting$eff_type_vec),
                                  isolate(all_rv$overall_setting$eff_cont_model),
                                  isolate(all_rv$overall_setting$eff_bin_model))
  })

  output$safe_block <- renderUI({
    all_rv$triggers$update_ER_dataset
    num <- all_rv$endpoint_num_setting$safe_num
    .build_group_models_ui_upload(ns, "safe", "Safety", num,
                                  isolate(all_rv$overall_setting$safe_type_vec),
                                  isolate(all_rv$overall_setting$safe_cont_model),
                                  isolate(all_rv$overall_setting$safe_bin_model))
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
