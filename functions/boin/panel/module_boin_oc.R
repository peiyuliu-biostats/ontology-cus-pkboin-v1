module_UI_boin_oc <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    uiOutput(ns("scenario_echo")),
    actionButton(ns("run"), "Run simulation"),
    uiOutput(ns("status_note")),
    tags$hr(),
    tags$h4("Selection probability & allocation"),
    DT::DTOutput(ns("oc_table")),
    tags$h4("Summary metrics"),
    uiOutput(ns("summary_card")),
    tags$h4("OBD selection probability"),
    plotOutput(ns("sel_plot"), height = "300px"),
    verbatimTextOutput(ns("early_stop"))
  )
}

boin_oc_signature <- function(ins) {
  paste(
    ins$method,
    paste(unlist(ins$design), collapse = ","),
    paste(unlist(ins$pk_design), collapse = ","),
    paste(unlist(ins$tite_design), collapse = ","),
    paste(ins$u, collapse = ","),
    paste(unlist(ins$trial), collapse = ","),
    paste(ins$p_true, collapse = ","),
    paste(ins$q_true, collapse = ","),
    paste(ins$r_true, collapse = ","),
    ins$n_rep,
    sep = "|"
  )
}

module_server_boin_oc <- function(input, output, session, boin_rv) {
  
  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))
  
  set_oc_controls_busy <- function(busy) {
    boin_rv$triggers$sim_running <- isTRUE(busy)
    fn <- if (isTRUE(busy)) shinyjs::disable else shinyjs::enable
    fn(selector = "#boin_oc-run")
    fn(selector = "#boin_sidebar-method input")
    fn(selector = "#boin_sidebar-simu_upload input")
    fn(selector = "#boin_sidebar-n_rep")
  }
  
  oc_inputs <- reactive({
    method <- boin_rv$overall_setting$method
    d <- boin_rv$design_setting
    tr <- boin_rv$trial_setting
    sc <- boin_rv$scenario_setting
    pk <- boin_rv$pk_setting
    tt <- boin_rv$tite_setting
    
    req(boin_method_implemented(method))
    req(length(sc$p_true) == tr$n_dose, length(sc$q_true) == tr$n_dose)
    if (boin_is_pk_method(method)) req(length(sc$r_true) == tr$n_dose)
    
    list(
      method = method,
      design = list(phi_T = d$phi_T, phi_E = d$phi_E, phi1 = d$phi1, phi2 = d$phi2,
                    CT = d$CT, CE = d$CE),
      pk_design = list(r_P = pk$r_P, r_I_mult = pk$r_I_mult, C_P = pk$C_P,
                       CV = pk$CV, g_P = pk$g_P,
                       zeta1 = pkboin_zeta1(pk$r_P, pk$r_I_mult)),
      tite_design = list(A_T = tt$A_T, A_E = tt$A_E,
                         accrual_rate = tt$accrual_rate,
                         suspend_threshold = tt$suspend_threshold,
                         use_susp = tt$use_susp,
                         accrual_random = tt$accrual_random),
      u = c(d$u1, d$u2, d$u3, d$u4),
      trial = list(n_dose = tr$n_dose, start_dose = tr$start_dose,
                   cohort_size = tr$cohort_size, n_max = tr$n_max),
      p_true = sc$p_true,
      q_true = sc$q_true,
      r_true = sc$r_true,
      n_rep = sc$n_rep
    )
  })
  
  current_signature <- reactive({
    boin_oc_signature(oc_inputs())
  })
  
  observeEvent(input$run, {
    ins <- oc_inputs()
    sig <- boin_oc_signature(ins)
    set_oc_controls_busy(TRUE)
    on.exit(set_oc_controls_busy(FALSE), add = TRUE)
    
    b <- boin_boundaries(ins$design$phi_T, phi1 = ins$design$phi1, phi2 = ins$design$phi2)
    label <- sprintf("Running %s OC (%d replications)", boin_method_label(ins$method), ins$n_rep)
    
    res <- tryCatch({
      withProgress(message = label, value = 0.05, {
        if (identical(ins$method, "TITE-PKBOIN-12")) {
          incProgress(0.15, detail = "Running TITE-PKBOIN12 operating characteristics.")
          oc_res <- tite_pkboin_operating_char(ins$p_true, ins$q_true, ins$r_true,
                                               ins$design, ins$pk_design, ins$tite_design,
                                               ins$trial, ins$u, n_rep = ins$n_rep)
          true_obd <- pkboin_true_obd(ins$p_true, ins$q_true, ins$r_true,
                                      list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E),
                                      b, ins$u, list(r_P = ins$pk_design$r_P))
          incProgress(0.7, detail = "Generating representative TITE trajectory.")
          traj <- tite_pkboin_one_trial_traj(ins$p_true, ins$q_true, ins$r_true,
                                             ins$design, ins$pk_design,
                                             ins$tite_design, ins$trial, b, ins$u)
        } else if (identical(ins$method, "PKBOIN-12")) {
          incProgress(0.25, detail = "Running PKBOIN12 operating characteristics.")
          oc_res <- pkboin_operating_char(ins$p_true, ins$q_true, ins$r_true,
                                          ins$design, ins$pk_design, ins$trial, ins$u,
                                          n_rep = ins$n_rep)
          true_obd <- pkboin_true_obd(ins$p_true, ins$q_true, ins$r_true,
                                      list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E),
                                      b, ins$u, list(r_P = ins$pk_design$r_P))
          incProgress(0.65, detail = "Generating representative PKBOIN12 trajectory.")
          traj <- pkboin_one_trial_traj(ins$p_true, ins$q_true, ins$r_true,
                                        ins$design, ins$pk_design,
                                        ins$trial, b, ins$u)
        } else {
          incProgress(0.35, detail = "Running BOIN12 operating characteristics.")
          oc_res <- boin_operating_char(ins$p_true, ins$q_true,
                                        ins$design, ins$trial, ins$u,
                                        n_rep = ins$n_rep)
          true_obd <- boin_true_obd(ins$p_true, ins$q_true,
                                    list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E),
                                    b, ins$u)
          incProgress(0.55, detail = "Generating representative BOIN12 trajectory.")
          traj <- boin_one_trial_traj(ins$p_true, ins$q_true,
                                      ins$design, ins$trial, b, ins$u)
        }
        incProgress(0.9, detail = "Finalizing cached results.")
        list(design = ins$design, pk_design = ins$pk_design,
             tite_design = ins$tite_design, trial = ins$trial,
             u = ins$u,
             scenario = list(p_true = ins$p_true, q_true = ins$q_true,
                             r_true = ins$r_true,
                             shape = boin_rv$scenario_setting$shape,
                             n_rep = ins$n_rep),
             method = ins$method, bounds = b, oc = oc_res,
             true_obd = true_obd, traj = traj$trajectory,
             traj_obd = traj$obd, traj_duration = traj$duration,
             signature = sig)
      })
    }, error = function(e) {
      showNotification(sprintf("Simulation failed: %s", conditionMessage(e)),
                       type = "error", duration = 12)
      NULL
    })
    
    if (is.null(res)) return(NULL)
    boin_rv$results$sim <- res
    boin_rv$triggers$sim_active <- TRUE
    boin_rv$triggers$sim_run_id <- boin_rv$triggers$sim_run_id + 1L
    boin_rv$triggers$sim_signature <- sig
    boin_rv$triggers$sim_method <- ins$method
  }, ignoreInit = TRUE)

  sim_result <- reactive({
    z <- boin_rv$results$sim
    req(!is.null(z))
    z
  })
  
  output$scenario_echo <- renderUI({
    ins <- oc_inputs()
    b <- boin_boundaries(ins$design$phi_T, phi1 = ins$design$phi1, phi2 = ins$design$phi2)
    true_obd <- tryCatch({
      if (boin_is_pk_method(ins$method)) {
        pkboin_true_obd(ins$p_true, ins$q_true, ins$r_true,
                        list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E),
                        b, ins$u, list(r_P = ins$pk_design$r_P))
      } else {
        boin_true_obd(ins$p_true, ins$q_true,
                      list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E),
                      b, ins$u)
      }
    }, error = function(e) NA_integer_)
    
    tags$div(
      style = "background:#f6f6f8; border-radius:6px; padding:8px 12px; margin-bottom:10px; font-size:13px; color:#333;",
      sprintf("Current scenario: %s, %d doses, start dose %d, cohort size %d, N_max %d, replications %d%s%s",
              boin_method_label(ins$method), ins$trial$n_dose, ins$trial$start_dose,
              ins$trial$cohort_size, ins$trial$n_max, ins$n_rep,
              if (!is.na(true_obd)) sprintf(", true OBD = dose %d", true_obd) else "",
              if (boin_is_pk_method(ins$method)) sprintf(", zeta1 = %.0f", ins$pk_design$zeta1) else "")
    )
  })
  
  output$status_note <- renderUI({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    sig <- current_signature()
    if (isTRUE(boin_rv$triggers$sim_running)) {
      tags$p(style = "color:#a05a00; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u25b6 Simulation is running. Method, mode, and replication controls are locked until this run finishes.")
    } else if (is.null(boin_rv$results$sim)) {
      tags$p(style = "color:#a05a00; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u25b6 Click \"Run simulation\" to compute results for the current settings.")
    } else if (!identical(boin_rv$triggers$sim_signature, sig)) {
      tags$p(style = "color:#a05a00; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u26a0 Settings changed. Click \"Run simulation\" again to update OC and Data.")
    } else {
      tags$p(style = "color:#1D9E75; font-size:12.5px; margin-top:6px; font-weight:600;",
             "\u2713 Results are current for these settings.")
    }
  })
  
  output$oc_table <- DT::renderDT({
    a <- sim_result()
    ins <- list(method = a$method, design = a$design, pk_design = a$pk_design,
                tite_design = a$tite_design, u = a$u,
                trial = a$trial, p_true = a$scenario$p_true,
                q_true = a$scenario$q_true, r_true = a$scenario$r_true)
    o <- a$oc
    D <- ins$trial$n_dose
    
    tr <- boin_scenario_truth(ins$p_true, ins$q_true,
                              list(phi_T = ins$design$phi_T, phi_E = ins$design$phi_E,
                                   phi1 = ins$design$phi1, phi2 = ins$design$phi2),
                              ins$u)
    tb <- tr$table
    
    df <- data.frame(
      dose = 1:D,
      p_true = tb$p_true,
      q_true = tb$q_true,
      EU_d = tb$EU_d,
      overdose = ifelse(o$overdose, "\u26a0", ""),
      selection_pct = round(o$selection_pct[paste0("dose", 1:D)], 1),
      mean_alloc = round(o$mean_alloc, 2),
      true_OBD = ifelse(!is.na(o$true_obd) & (1:D) == o$true_obd, "\u2605", "")
    )
    
    if (boin_is_pk_method(ins$method)) {
      df$r_true <- round(ins$r_true, 1)
      df$pk_adequate <- ifelse(ins$r_true > ins$pk_design$zeta1, "yes", "no")
      df <- df[, c("dose", "p_true", "q_true", "r_true", "pk_adequate",
                   "EU_d", "overdose", "selection_pct", "mean_alloc", "true_OBD")]
    }
    
    DT::datatable(df, rownames = FALSE, selection = "none",
                  options = list(dom = "t", ordering = FALSE, scrollX = TRUE,
                                 destroy = TRUE, autoWidth = TRUE))
  }, server = FALSE)
  
  output$summary_card <- renderUI({
    a <- sim_result()
    ins <- list(method = a$method)
    o <- a$oc
    
    fmt <- function(x, pct = FALSE) {
      if (is.null(x) || is.na(x)) return("\u2014")
      if (pct) sprintf("%.1f%%", x) else sprintf("%.2f", x)
    }
    row <- function(k, v) tags$tr(
      tags$td(style = "padding:3px 14px 3px 0; color:#444;", k),
      tags$td(style = "padding:3px 0; font-weight:600; text-align:right;", v)
    )
    
    rows <- list(
      row("Correct OBD selection %", fmt(o$correct_sel_pct, TRUE)),
      row("# patients at OBD (mean)", fmt(o$n_at_obd)),
      row("# patients at overdoses (mean)", fmt(o$n_at_overdose)),
      row("Risk of poor allocation %", fmt(o$poor_alloc_pct, TRUE)),
      row("Early-stop %", fmt(o$early_stop_pct, TRUE)),
      row("Mean # DLTs", fmt(o$mean_dlt)),
      row("Mean # responses", fmt(o$mean_eff))
    )
    
    if (boin_is_pk_method(ins$method)) {
      rows <- c(rows, list(
        row("PK-driven early termination %", fmt(o$pk_early_term_pct, TRUE)),
        row("Mean # PK-eliminated doses", fmt(o$mean_pk_elim)),
        row("zeta1", fmt(o$zeta1))
      ))
    }
    if (identical(ins$method, "TITE-PKBOIN-12")) {
      rows <- c(rows, list(
        row("Mean duration (days)", fmt(o$mean_duration_days)),
        row("Mean duration (months)", fmt(o$mean_duration_months))
      ))
    }
    
    tags$table(style = "border-collapse:collapse; font-size:13.5px; min-width:340px;", rows)
  })
  
  output$sel_plot <- renderPlot({
    a <- sim_result()
    o <- a$oc
    D <- a$trial$n_dose
    df <- data.frame(dose = factor(1:D),
                     pct = o$selection_pct[paste0("dose", 1:D)])
    ggplot2::ggplot(df, ggplot2::aes(dose, pct)) +
      ggplot2::geom_col(fill = "#7F77DD") +
      ggplot2::labs(x = "Dose level", y = "Selection %",
                    title = "OBD selection probability") +
      ggplot2::theme_minimal(base_size = 15)
  })
  
  output$early_stop <- renderPrint({
    a <- sim_result()
    o <- a$oc
    cat(sprintf("Early stop (no dose selected): %.1f%%\n", o$early_stop_pct))
    if (boin_is_pk_method(a$method)) {
      cat(sprintf("PK-driven early termination: %.1f%%\n", o$pk_early_term_pct))
      cat(sprintf("Mean # PK-eliminated doses: %.2f\n", o$mean_pk_elim))
    }
    if (identical(a$method, "TITE-PKBOIN-12")) {
      cat(sprintf("Mean trial duration: %.2f days (%.2f months)\n",
                  o$mean_duration_days, o$mean_duration_months))
    }
  })
}
