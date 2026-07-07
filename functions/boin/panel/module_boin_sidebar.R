# =====================================================================
# BOIN sidebar module (Settings panel) -- STAGE B
# ---------------------------------------------------------------------
# Extends the stage A2 skeleton (method selector + mode toggle + PK/
# PK/TITE cards, unchanged in structure/condition below) with the full
# BOIN-12 design/trial/scenario inputs. PKBOIN-12 and TITE-PKBOIN-12 use
# their dedicated scientific engines while sharing this sidebar state.
#
# Length-compression choices (per prior discussion): (1) bsCollapse
# accordion (shinyBS, already an app dependency) so most sections can
# be collapsed; (2) Utility Table laid out as a 2x2 fluidRow/column(6)
# grid rather than 4 stacked full-width inputs; (3) per-dose truth
# generation parameters live in the Scenario tab, not here (same
# convention as STEIN's sidebar keeping only the "shape" selector here
# and the actual p_true/q_true table in the Scenario tab).
# =====================================================================

boin_help_label <- function(text, tip) {
  tags$span(
    text,
    tags$span(class = "cus-help", "?",
              tags$span(class = "cus-tip", tip))
  )
}

module_UI_boin_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("method"),
      label = boin_help_label(
        "Method",
        "BOIN12: toxicity + efficacy utility-based interval design, no PK. PKBOIN12: adds a pharmacokinetic admissibility criterion. TITE-PKBOIN12: adds time-to-event handling for late-onset outcomes on top of PKBOIN12."),
      choices = list("BOIN12" = "BOIN12",
                     "PKBOIN12" = "PKBOIN-12",
                     "TITE-PKBOIN12" = "TITE-PKBOIN-12"),
      selected = "BOIN12", width = "100%"
    ),
    
    radioButtons(
      inputId = ns("simu_upload"),
      label = div(class = "custom-label", "Simulate or upload trial data?"),
      choices = list("Simulate trial data" = 1, "Upload trial data" = 2),
      selected = 1, width = "100%"
    ),
    
    tags$hr(style = "margin:8px 0;"),
    
    # ---- upload branch, part 1: upload trial data ----
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 2"),
      tags$h4(style = "margin:8px 0 6px;", "Upload trial data"),
      fileInput(ns("trial_upload"), "Upload trial data:",
                accept = c(".csv", ".xlsx", "text/csv", "text/comma-separated-values",
                           "text/plain", ".txt", ".rds")),
      helpText(
        "Data Requirements:",
        tags$ol(
          tags$li("Acceptable formats: .csv, .xlsx, .txt, .rds;"),
          tags$li("Cohort-level (recommended): columns ",
                  tags$code("cohort"), ", ", tags$code("dose"), ", ", tags$code("n"), ", ",
                  tags$code("n1"), ", ", tags$code("n2"), ", ", tags$code("n3"), ", ", tags$code("n4"),
                  " -- the joint outcome counts (n1=efficacy&no-toxicity, n2=efficacy&toxicity, n3=no-efficacy&no-toxicity, n4=no-efficacy&toxicity; n1+n2+n3+n4 must equal n), one row per cohort, in enrollment order;"),
          tags$li("Or patient-level (auto-aggregated to cohort-level): columns ",
                  tags$code("patient_id"), ", ", tags$code("cohort"), ", ", tags$code("dose"), ", ",
                  tags$code("dlt"), ", ", tags$code("response"), " (each 0/1);"),
          tags$li("For PKBOIN12, patient-level data must also include ", tags$code("pk"),
                  ". For TITE-PKBOIN12, upload patient-level data with ", tags$code("enroll"),
                  ", ", tags$code("pk"), ", and either ", tags$code("tox_time"), "/", tags$code("eff_time"),
                  " as days after enrollment or ", tags$code("tox_event"), "/", tags$code("eff_event"),
                  " as absolute event times; event times are required only for observed events."),
          tags$li("dose must be between 1 and the number of doses set below. Files with structural problems are rejected rather than partially imported.")
        )
      ),
      uiOutput(ns("upload_status")),
      tags$hr(style = "margin:8px 0;")
    ),
    
    # ---- upload branch, part 2: dose count ----
    conditionalPanel(
      condition = paste0("input['", ns("simu_upload"), "'] == 2"),
      tags$h4(style = "margin:8px 0 6px;", "Dose count"),
      numericInput(ns("n_dose_upload"),
                   boin_help_label("Number of doses",
                                   "The trial's total planned dose levels, not just how many appear in your uploaded data -- a dose level the trial hasn't reached yet won't appear in the data but still needs to exist for escalation to be possible."),
                   value = 5, min = 2, max = 10, step = 1),
      tags$hr(style = "margin:8px 0;")
    ),
    
    # ---- shared: Admissible Criteria + Utility Table (bsCollapse, open by default) ----
    bsCollapse(id = ns("acc"), open = "core",
               bsCollapsePanel(
                 title = "Admissible Criteria & Utility", value = "core",
                 fluidRow(
                   column(6, numericInput(ns("phi_T"),
                                          boin_help_label("phi_T", "Target toxicity upper limit. Drives the toxicity boundaries \u03bbe/\u03bbd (and \u03c61/\u03c62) shown in Design/Flowchart -- these are a pure function of \u03c6T by the BOIN formula and do NOT depend on \u03c6E."),
                                          value = 0.35, min = 0.05, max = 0.6, step = 0.01)),
                   column(6, numericInput(ns("phi_E"),
                                          boin_help_label("phi_E", "Efficacy lower limit (futility threshold). Does NOT move the toxicity boundaries \u03bbe/\u03bbd. It drives: the futility elimination test Pr(q_d<\u03c6E|data)>CE, the RDS utility benchmark u_b (Design/Flowchart), and the OBD efficacy-admissibility test q_true>\u03c6E."),
                                          value = 0.25, min = 0.05, max = 0.9, step = 0.01))
                 ),
                 fluidRow(
                   column(6, numericInput(ns("CT"),
                                          boin_help_label("CT", "Toxicity elimination posterior cutoff."),
                                          value = 0.95, min = 0.5, max = 0.999, step = 0.01)),
                   column(6, numericInput(ns("CE"),
                                          boin_help_label("CE", "Efficacy (futility) elimination posterior cutoff."),
                                          value = 0.90, min = 0.5, max = 0.999, step = 0.01))
                 ),
                 tags$p(style = "margin:8px 0 2px; font-weight:600; font-size:13px;", "Utility table (2x2 joint outcome)"),
                 fluidRow(
                   column(6, numericInput(ns("u1"), boin_help_label("u1", "Utility: efficacy & no-toxicity (best outcome; paper O1). Default 100. The four utilities are independent scores on a 0-100 scale; they need NOT sum to any fixed total, and changing one does not require changing another (per BOIN12, u2+u3 = 100 is only an example, not a constraint)."), value = 100, step = 1)),
                   column(6, numericInput(ns("u2"), boin_help_label("u2", "Utility: efficacy & toxicity (paper O3). Default 60. Independent of u1/u3/u4 (need not sum to 100)."), value = 60, step = 1))
                 ),
                 fluidRow(
                   column(6, numericInput(ns("u3"), boin_help_label("u3", "Utility: no-efficacy & no-toxicity (paper O2). Default 40. Independent of u1/u2/u4 (need not sum to 100)."), value = 40, step = 1)),
                   column(6, numericInput(ns("u4"), boin_help_label("u4", "Utility: no-efficacy & toxicity (worst outcome; paper O4). Default 0. Independent of the other three."), value = 0, step = 1))
                 ),
                 tags$p(style = "color:#555; font-size:11.5px; margin:6px 0 0;",
                        "Dose desirability follows BOIN12's utility / rank-based desirability score (RDS); there is no separate objective toggle.")
               )
    ),
    
    tags$hr(style = "margin:8px 0;"),
    
    # ---- PK Setting (PKBOIN12 / TITE-PKBOIN12; shown when method != BOIN12) ----
    conditionalPanel(
      condition = paste0("input['", ns("method"), "'] != 'BOIN12'"),
      tags$div(
        style = "background:#fff6e6; border-radius:6px; padding:8px 12px; margin-bottom:10px;",
        tags$h4(style = "margin:4px 0 6px;", "PK Setting"),
        fluidRow(
          column(6, numericInput(ns("r_P"),
                                 boin_help_label("r_P", "Target PK value (e.g. AUC) on the PK-outcome's own scale -- there is NO universal default, so review it for your endpoint. Drives the PK cutoff \u03b61 = (r_P + r_I)/2 and the PK elimination test Pr(r_d < r_P | data) > C_P."),
                                 value = 6000, min = 1, step = 100)),
          column(6, numericInput(ns("r_I_mult"),
                                 boin_help_label("r_I / r_P", "Inefficacious-PK multiplier: r_I = (this) \u00d7 r_P. Paper default 0.6, giving \u03b61 = 0.8\u00b7r_P. Must be in (0,1)."),
                                 value = 0.6, min = 0.05, max = 0.99, step = 0.05))
        ),
        fluidRow(
          column(6, numericInput(ns("C_P"),
                                 boin_help_label("C_P", "PK elimination posterior cutoff. A dose with n\u2265 6 and Pr(r_d < r_P | data) > C_P is flagged as having inefficacious PK exposure. Paper default 0.95."),
                                 value = 0.95, min = 0.5, max = 0.999, step = 0.01)),
          column(6, numericInput(ns("CV"),
                                 boin_help_label("CV", "Coefficient of variation of the individual-level PK outcome; individual PK sd = CV \u00d7 r_d. Used for the PK posterior and for individual-level PK generation in simulation. Paper default 0.25."),
                                 value = 0.25, min = 0.01, max = 1, step = 0.05))
        ),
        fluidRow(
          column(6, numericInput(ns("g_P"),
                                 boin_help_label("g_P", "PK-to-outcome scaling ratio linking individual PK to individual toxicity/efficacy: p_{d,j}=min{p_d(1+g_P(r_{d,j}-r_d)/r_d),1}. g_P = 0 makes PK uncorrelated with tox/eff (design still excludes low-PK doses, degrading gracefully toward BOIN12). Paper default 1."),
                                 value = 1, min = 0, max = 5, step = 0.5)),
          column(6,
                 tags$div(style = "padding-top:24px; font-family:'Courier New',monospace; font-size:12px; color:#a05a00;",
                          uiOutput(ns("zeta1_readout"))))
        ),
        tags$p(style = "color:#a05a00; font-size:11px; margin:4px 0 0;",
               "\u03b61 (PK cutoff) and r_I are derived read-only from r_P and the multiplier.")
      )
    ),
    
    # ---- TITE Setting (TITE-PKBOIN12; simulate mode) ----
    conditionalPanel(
      condition = paste0("input['", ns("method"), "'] == 'TITE-PKBOIN-12'"),
      tags$div(
        style = "background:#fdeceb; border-radius:6px; padding:8px 12px; margin-bottom:10px;",
        tags$h4(style = "margin:4px 0 6px;", "TITE Setting"),
        fluidRow(
          column(6, numericInput(ns("A_T"),
                                 boin_help_label("A_T", "Toxicity assessment window in days. Paper simulation default: 30."),
                                 value = 30, min = 1, step = 1)),
          column(6, numericInput(ns("A_E"),
                                 boin_help_label("A_E", "Efficacy assessment window in days. Paper simulation default: 60."),
                                 value = 60, min = 1, step = 1))
        ),
        fluidRow(
          column(6, numericInput(ns("accrual_rate"),
                                 boin_help_label("Accrual interval", "Days between enrolled patients under deterministic accrual. Paper example uses 10 days per patient."),
                                 value = 10, min = 0.1, step = 1)),
          column(6, numericInput(ns("suspend_threshold"),
                                 boin_help_label("Suspension cutoff", "Suspend accrual until at least floor(n_d * cutoff + 1) patients at the current dose have toxicity and efficacy information."),
                                 value = 0.5, min = 0.01, max = 1, step = 0.05))
        ),
        checkboxInput(ns("use_susp"), "Apply accrual suspension rule", value = TRUE),
        checkboxInput(ns("accrual_random"), "Random accrual intervals", value = FALSE)
      )
    ),
    
    tags$hr(style = "margin:8px 0;"),
    
    # ---- simulate branch: Dose & Sample Size + Scenario shape/reps ----
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
    )
  )
}

module_server_boin_sidebar <- function(input, output, session, boin_rv) {
  
  upload_status <- reactiveVal(NULL)
  
  observeEvent(input$method, {
    boin_rv$overall_setting$method <- input$method
  })
  observeEvent(input$simu_upload, {
    boin_rv$overall_setting$simu_or_not <- as.integer(input$simu_upload)
  })
  
  # dose count: whichever mode's own input is active (same convention as STEIN)
  observe({
    req(input$simu_upload)
    if (input$simu_upload == 1) {
      req(input$n_dose, input$start_dose, input$cohort_size, input$n_max)
      boin_rv$trial_setting$n_dose      <- as.integer(input$n_dose)
      boin_rv$trial_setting$start_dose  <- as.integer(input$start_dose)
      boin_rv$trial_setting$cohort_size <- as.integer(input$cohort_size)
      boin_rv$trial_setting$n_max       <- as.integer(input$n_max)
    } else {
      req(input$n_dose_upload)
      boin_rv$trial_setting$n_dose <- as.integer(input$n_dose_upload)
    }
  })
  
  # design parameters
  observe({
    req(input$phi_T, input$phi_E, input$CT, input$CE,
        input$u1, input$u2, input$u3, input$u4)
    boin_rv$design_setting$phi_T <- input$phi_T
    boin_rv$design_setting$phi_E <- input$phi_E
    boin_rv$design_setting$CT    <- input$CT
    boin_rv$design_setting$CE    <- input$CE
    boin_rv$design_setting$u1    <- input$u1
    boin_rv$design_setting$u2    <- input$u2
    boin_rv$design_setting$u3    <- input$u3
    boin_rv$design_setting$u4    <- input$u4
  })
  
  # scenario shape + replications
  observe({
    req(input$shape, input$n_rep)
    boin_rv$scenario_setting$shape <- input$shape
    boin_rv$scenario_setting$n_rep <- as.integer(input$n_rep)
  })
  
  # ---- PK Setting -> pk_setting (with derived r_I / zeta1) --------------
  # The pure functions recompute zeta1 internally from (r_P, r_I_mult), so
  # the derived fields stored here are for display only and can never
  # corrupt a decision even if momentarily stale. Guards keep r_I_mult in
  # (0,1) and r_P > 0; invalid entries are ignored (req) rather than
  # written, so a half-typed value never propagates.
  observe({
    req(input$r_P, input$r_I_mult, input$C_P, input$CV, input$g_P)
    req(input$r_P > 0, input$r_I_mult > 0, input$r_I_mult < 1)
    boin_rv$pk_setting$r_P      <- input$r_P
    boin_rv$pk_setting$r_I_mult <- input$r_I_mult
    boin_rv$pk_setting$C_P      <- input$C_P
    boin_rv$pk_setting$CV       <- input$CV
    boin_rv$pk_setting$g_P      <- input$g_P
    boin_rv$pk_setting$r_I      <- input$r_I_mult * input$r_P
    boin_rv$pk_setting$zeta1    <- (1 + input$r_I_mult) / 2 * input$r_P
  })
  
  output$zeta1_readout <- renderUI({
    req(input$r_P, input$r_I_mult)
    req(input$r_P > 0, input$r_I_mult > 0, input$r_I_mult < 1)
    z  <- (1 + input$r_I_mult) / 2 * input$r_P
    rI <- input$r_I_mult * input$r_P
    HTML(sprintf("r_I = %.0f<br/>\u03b61 = %.0f", rI, z))
  })

  # ---- TITE Setting -> tite_setting -----------------------------------
  observe({
    req(input$A_T, input$A_E, input$accrual_rate, input$suspend_threshold)
    req(input$A_T > 0, input$A_E > 0, input$accrual_rate > 0,
        input$suspend_threshold > 0, input$suspend_threshold <= 1)
    boin_rv$tite_setting$A_T <- input$A_T
    boin_rv$tite_setting$A_E <- input$A_E
    boin_rv$tite_setting$accrual_rate <- input$accrual_rate
    boin_rv$tite_setting$suspend_threshold <- input$suspend_threshold
    boin_rv$tite_setting$use_susp <- isTRUE(input$use_susp)
    boin_rv$tite_setting$accrual_random <- isTRUE(input$accrual_random)
  })
  
  # ---- upload validation ----
  observeEvent(input$trial_upload, {
    req(input$trial_upload)
    req(input$simu_upload == 2)
    
    file_path <- input$trial_upload$datapath
    file_ext  <- tolower(tools::file_ext(file_path))
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
    method <- boin_rv$overall_setting$method
    result <- if (identical(method, "TITE-PKBOIN-12")) {
      tite_design <- list(A_T = input$A_T, A_E = input$A_E,
                          accrual_rate = input$accrual_rate,
                          suspend_threshold = input$suspend_threshold,
                          use_susp = isTRUE(input$use_susp),
                          accrual_random = isTRUE(input$accrual_random))
      tite_pkboin_validate_upload(raw, D, tite_design)
    } else if (identical(method, "PKBOIN-12")) {
      pkboin_validate_upload(raw, D)
    } else {
      boin_validate_upload(raw, D)
    }
    
    if (!result$ok) {
      showNotification(
        paste0("Upload rejected - ", paste(result$errors, collapse = " ")),
        type = "error", duration = 12
      )
      upload_status(list(ok = FALSE, errors = result$errors, n = 0))
      return(NULL)
    }
    
    boin_rv$trial_data$upload_raw <- as.data.frame(raw)
    boin_rv$trial_data$upload_method <- method
    if (method %in% c("PKBOIN-12", "TITE-PKBOIN-12")) {
      boin_rv$trial_data$patient_data <- result$patients
      boin_rv$trial_data$cohort_data  <- result$cohort_data
      n_loaded <- nrow(result$cohort_data)
    } else {
      boin_rv$trial_data$patient_data <- NULL
      boin_rv$trial_data$cohort_data  <- result$data
      n_loaded <- nrow(result$data)
    }
    
    if (length(result$warnings) > 0) {
      showNotification(paste(result$warnings, collapse = " "), type = "warning", duration = 10)
    }
    showNotification(sprintf("Trial data uploaded: %d cohort(s) loaded.", n_loaded), type = "message")
    upload_status(list(ok = TRUE, errors = character(0), n = n_loaded))
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
}
