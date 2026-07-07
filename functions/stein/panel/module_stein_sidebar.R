# =====================================================================
# STEIN sidebar module (Settings panel)
# ---------------------------------------------------------------------
# Mode toggle (simulate/upload) + shared Design Parameters (phi0, psi1,
# psi2, phi1/phi2 mult, w1, w2, CT, CE -- these drive the derived
# boundaries used by Design/Flowchart in both modes, and by Trial
# Conduct/OBD Selection in upload mode, so they are always visible,
# not hidden inside either mode's branch), then mode-specific inputs:
# simulate gets trial structure + scenario controls; upload gets a
# dose-count input + file upload. Mirrors the CUS module_UI/server
# convention and reuses its .cus-help tooltip styling.
# =====================================================================

# small helper: label with a .cus-help tooltip (reuses app-wide CSS class)
stein_help_label <- function(text, tip) {
  tags$span(
    text,
    tags$span(class = "cus-help", "?",
              tags$span(class = "cus-tip", tip))
  )
}

module_UI_stein_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("simu_upload"),
      label = div(class = "custom-label", "Simulate or upload trial data?"),
      choices = list("Simulate trial data" = 1, "Upload trial data" = 2),
      selected = 1, width = "100%"
    ),

    # ---- upload branch, part 1: upload trial data (shown first in upload mode) ----
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 2"),

      tags$h4(style = "margin:8px 0 6px;", "Upload trial data"),
      fileInput(ns("trial_upload"), "Upload cohort-level trial data:",
                accept = c(".csv", ".xlsx", "text/csv", "text/comma-separated-values",
                          "text/plain", ".txt", ".rds")),
      helpText(
        "Data Requirements:",
        tags$ol(
          tags$li("Acceptable formats: .csv, .xlsx, .txt, .rds;"),
          tags$li("Cohort-level (recommended): columns ",
                  tags$code("cohort"), ", ", tags$code("dose"), ", ", tags$code("n"), ", ",
                  tags$code("n_dlt"), ", ", tags$code("n_eff"),
                  " (one row per cohort, in enrollment order);"),
          tags$li("Or patient-level (auto-aggregated to cohort-level): columns ",
                  tags$code("patient_id"), ", ", tags$code("cohort"), ", ", tags$code("dose"), ", ",
                  tags$code("dlt"), ", ", tags$code("response"), " (each 0/1);"),
          tags$li("dose must be between 1 and the number of doses set below; n_dlt/n_eff cannot exceed n. Files with such problems are rejected rather than partially imported.")
        )
      ),
      uiOutput(ns("upload_status"))
    ),

    # ---- shared: Design Parameters (both modes need these -- they drive
    #      the derived boundaries phiL/phiU/psi used by Design/Flowchart,
    #      and by Trial Conduct/OBD Selection in upload mode) ----
    tags$hr(style = "margin:8px 0;"),
    tags$h4(style = "margin:8px 0 6px;", "Design Parameters"),
    fluidRow(
      column(6, numericInput(ns("phi0"),
        stein_help_label("phi0", "Target toxicity probability (max acceptable DLT rate)."),
        value = 0.35, min = 0.05, max = 0.6, step = 0.01)),
      column(6, numericInput(ns("psi1"),
        stein_help_label("psi1", "Clinically uninteresting efficacy response rate."),
        value = 0.30, min = 0.05, max = 0.9, step = 0.01))
    ),
    fluidRow(
      column(6, numericInput(ns("psi2"),
        stein_help_label("psi2", "Clinically desirable efficacy response rate (psi2 > psi1)."),
        value = 0.80, min = 0.1, max = 0.99, step = 0.01)),
      column(6, numericInput(ns("phi1_mult"),
        stein_help_label("phi1 mult", "Lower toxicity anchor as a multiple of phi0 (default 0.75)."),
        value = 0.75, min = 0.1, max = 0.99, step = 0.05))
    ),
    fluidRow(
      column(6, numericInput(ns("phi2_mult"),
        stein_help_label("phi2 mult", "Upper toxicity anchor as a multiple of phi0 (default 1.25)."),
        value = 1.25, min = 1.01, max = 2, step = 0.05)),
      column(6, numericInput(ns("w1"),
        stein_help_label("w1", "Utility weight penalizing toxicity."),
        value = 0.33, min = 0, step = 0.01))
    ),
    fluidRow(
      column(6, numericInput(ns("w2"),
        stein_help_label("w2", "Extra utility penalty when p_tilde exceeds phi0."),
        value = 1.09, min = 0, step = 0.01)),
      column(6, numericInput(ns("CT"),
        stein_help_label("CT", "Toxicity elimination posterior cutoff."),
        value = 0.95, min = 0.5, max = 0.999, step = 0.01))
    ),
    fluidRow(
      column(6, numericInput(ns("CE"),
        stein_help_label("CE", "Efficacy (futility) elimination posterior cutoff."),
        value = 0.98, min = 0.5, max = 0.999, step = 0.01))
    ),
    tags$hr(style = "margin:8px 0;"),

    # ---- simulate branch ----
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 1"),

      tags$h4(style = "margin:8px 0 6px;", "Dose & Sample Size"),
      fluidRow(
        column(6, numericInput(ns("n_dose"), "Number of doses", value = 5, min = 2, max = 10, step = 1)),
        column(6, numericInput(ns("start_dose"), "Starting dose", value = 1, min = 1, step = 1))
      ),
      fluidRow(
        column(6, numericInput(ns("cohort_size"), "Cohort size", value = 3, min = 1, step = 1)),
        column(6, numericInput(ns("n_max"), "Max sample size", value = 30, min = 3, step = 1))
      ),

      tags$hr(style = "margin:8px 0;"),
      tags$h4(style = "margin:8px 0 6px;", "Scenario (true rates)"),
      selectInput(ns("shape"), "Efficacy shape",
        choices = c("increasing", "plateau", "unimodal", "constant"),
        selected = "unimodal"),
      numericInput(ns("n_rep"), "Replications", value = 2000, min = 100, max = 10000, step = 100),
      helpText("Per-dose true toxicity/efficacy are editable in the Scenario tab.")
    ),

    # ---- upload branch, part 2: dose count (shown after Design Parameters) ----
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 2"),

      tags$h4(style = "margin:8px 0 6px;", "Dose count"),
      numericInput(ns("n_dose_upload"),
        stein_help_label("Number of doses",
          "The trial's total planned dose levels, not just how many appear in your uploaded data. This can't be inferred from the file: a dose level the trial hasn't reached yet (e.g. dose 5 of 5) simply won't appear in the data, but it still needs to exist for escalation to be possible. Set this to match your protocol's dose count."),
        value = 5, min = 2, max = 10, step = 1)
    )
  )
}

module_server_stein_sidebar <- function(input, output, session, stein_rv) {

  upload_status <- reactiveVal(NULL)

  # push simu/upload flag
  observeEvent(input$simu_upload, {
    stein_rv$overall_setting$simu_or_not <- as.integer(input$simu_upload)
  })

  # push trial structure -- n_dose comes from whichever mode's own input is active;
  # start_dose/cohort_size/n_max only apply to simulate mode (trial *generation*),
  # so they are left untouched (and unused) in upload mode.
  observe({
    req(input$simu_upload)
    if (input$simu_upload == 1) {
      req(input$n_dose, input$start_dose, input$cohort_size, input$n_max)
      stein_rv$trial_setting$n_dose      <- as.integer(input$n_dose)
      stein_rv$trial_setting$start_dose  <- as.integer(input$start_dose)
      stein_rv$trial_setting$cohort_size <- as.integer(input$cohort_size)
      stein_rv$trial_setting$n_max       <- as.integer(input$n_max)
    } else {
      req(input$n_dose_upload)
      stein_rv$trial_setting$n_dose <- as.integer(input$n_dose_upload)
    }
  })

  # ---- upload validation: reject (do not import) anything that would
  #      break downstream computation; see fun_stein_upload_validate.R ----
  observeEvent(input$trial_upload, {
    req(input$trial_upload)
    req(input$simu_upload == 2)

    file_path <- input$trial_upload$datapath
    file_ext  <- tools::file_ext(file_path)
    raw <- switch(
      file_ext,
      "csv"  = read.csv(file_path, header = TRUE),
      "txt"  = read.csv(file_path, header = TRUE),
      "xlsx" = readxl::read_excel(file_path),
      "rds"  = readRDS(file_path),
      { showNotification("Unsupported file type.", type = "error"); NULL }
    )
    if (is.null(raw)) return(NULL)

    D <- as.integer(input$n_dose_upload)
    result <- stein_validate_upload(raw, D)

    if (!result$ok) {
      showNotification(
        paste0("Upload rejected \u2014 ", paste(result$errors, collapse = " ")),
        type = "error", duration = 12
      )
      upload_status(list(ok = FALSE, errors = result$errors, n = 0))
      return(NULL)
    }

    stein_rv$trial_data$cohort_data <- result$data
    stein_rv$trial_data$upload_raw  <- raw

    if (length(result$warnings) > 0) {
      showNotification(paste(result$warnings, collapse = " "), type = "warning", duration = 10)
    }
    showNotification(sprintf("Trial data uploaded: %d cohort(s) loaded.", nrow(result$data)), type = "message")
    upload_status(list(ok = TRUE, errors = character(0), n = nrow(result$data)))
  })

  output$upload_status <- renderUI({
    s <- upload_status()
    if (is.null(s)) return(NULL)
    if (s$ok) {
      tags$p(style = "color:#1D9E75; font-size:12px; margin-top:6px;",
             sprintf("\u2713 %d cohort(s) loaded.", s$n))
    } else {
      tags$div(style = "color:#A32D2D; font-size:12px; margin-top:6px;",
               lapply(s$errors, function(e) tags$p(e)))
    }
  })

  # push design parameters
  observe({
    req(input$phi0, input$psi1, input$psi2)
    stein_rv$design_setting$phi0      <- input$phi0
    stein_rv$design_setting$psi1      <- input$psi1
    stein_rv$design_setting$psi2      <- input$psi2
    stein_rv$design_setting$phi1_mult <- input$phi1_mult
    stein_rv$design_setting$phi2_mult <- input$phi2_mult
    stein_rv$design_setting$phi1      <- input$phi1_mult * input$phi0
    stein_rv$design_setting$phi2      <- input$phi2_mult * input$phi0
    stein_rv$design_setting$w1        <- input$w1
    stein_rv$design_setting$w2        <- input$w2
    stein_rv$design_setting$CT        <- input$CT
    stein_rv$design_setting$CE        <- input$CE
  })

  # push scenario shape + replications
  observe({
    req(input$shape, input$n_rep)
    stein_rv$scenario_setting$shape <- input$shape
    stein_rv$scenario_setting$n_rep <- as.integer(input$n_rep)
  })
}
