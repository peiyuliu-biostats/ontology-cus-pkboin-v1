# =====================================================================
# BOIN-12 / PKBOIN-12 Scenario tab module
# =====================================================================

boin_seed_truth <- function(shape, D) {
  tox <- seq(0.05, 0.55, length.out = D)
  eff <- switch(shape,
                increasing = seq(0.10, 0.60, length.out = D),
                plateau = pmin(seq(0.10, 0.60, length.out = D), 0.45),
                unimodal = {
                  peak <- ceiling(D / 2)
                  up <- seq(0.15, 0.55, length.out = peak)
                  dn <- seq(0.55, 0.30, length.out = D - peak + 1)[-1]
                  c(up, dn)
                },
                constant = rep(0.40, D)
  )
  data.frame(dose = 1:D, p_true = round(tox, 3), q_true = round(eff[1:D], 3))
}

boin_seed_pk_truth <- function(D, r_P) {
  round(seq(0.35 * r_P, 1.40 * r_P, length.out = D), 0)
}

boin_label_stack <- function(dose, label, y_top = 0.96, gap = 0.075) {
  df <- data.frame(dose = dose, label = label, stringsAsFactors = FALSE)
  df <- df[!is.na(df$dose) & !is.na(df$label) & nzchar(df$label), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame(dose = numeric(0), label = character(0), y = numeric(0)))
  df <- df[order(df$dose, df$label), , drop = FALSE]
  df$rank <- ave(df$dose, df$dose, FUN = seq_along)
  df$y <- pmax(0.08, y_top - (df$rank - 1) * gap)
  df
}

boin_is_pk_method <- function(method) {
  method %in% c("PKBOIN-12", "TITE-PKBOIN-12")
}

module_UI_boin_scenario <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    tags$h4("Per-dose true rates"),
    uiOutput(ns("pk_truth_tools")),
    helpText("Values are edited directly in the table below and persist. Changing efficacy shape reseeds toxicity and efficacy only."),
    DT::DTOutput(ns("truth_table")),
    tags$p(style = "color:#777; font-size:12px; margin-top:4px;",
           "Derived columns are read-only. For PKBOIN12 and TITE-PKBOIN12, r_true is a scenario truth and is not overwritten when PK settings change unless reset explicitly."),
    tags$h4("True dose-response curves"),
    plotOutput(ns("truth_plot"), height = "340px")
  )
}

module_server_boin_scenario <- function(input, output, session, boin_rv) {
  
  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))
  
  output$pk_truth_tools <- renderUI({
    req(boin_is_pk_method(boin_rv$overall_setting$method))
    tagList(
      actionButton(session$ns("reset_r_true"), "Reset PK truth from r_P"),
      tags$p(style = "color:#777; font-size:12px; margin:4px 0 8px;",
             "PK setting changes update zeta1, PK adequacy, plots, and simulations. They do not silently overwrite manually edited r_true.")
    )
  })
  
  observe({
    D <- boin_rv$trial_setting$n_dose
    req(D)
    if (length(boin_rv$scenario_setting$p_true) != D ||
        length(boin_rv$scenario_setting$q_true) != D) {
      tb <- boin_seed_truth(boin_rv$scenario_setting$shape, D)
      boin_rv$scenario_setting$p_true <- tb$p_true
      boin_rv$scenario_setting$q_true <- tb$q_true
    }
    if (length(boin_rv$scenario_setting$r_true) != D) {
      boin_rv$scenario_setting$r_true <- boin_seed_pk_truth(D, boin_rv$pk_setting$r_P)
    }
  })
  
  observeEvent(boin_rv$scenario_setting$shape, {
    D <- boin_rv$trial_setting$n_dose
    req(D)
    tb <- boin_seed_truth(boin_rv$scenario_setting$shape, D)
    boin_rv$scenario_setting$p_true <- tb$p_true
    boin_rv$scenario_setting$q_true <- tb$q_true
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_r_true, {
    D <- boin_rv$trial_setting$n_dose
    req(D, boin_rv$pk_setting$r_P)
    boin_rv$scenario_setting$r_true <- boin_seed_pk_truth(D, boin_rv$pk_setting$r_P)
  })
  
  truth <- reactive({
    D <- boin_rv$trial_setting$n_dose
    p <- boin_rv$scenario_setting$p_true
    q <- boin_rv$scenario_setting$q_true
    req(length(p) == D, length(q) == D)
    d <- boin_rv$design_setting
    req(d$phi_T, d$phi_E)
    u <- c(d$u1, d$u2, d$u3, d$u4)
    boin_scenario_truth(p, q,
                        list(phi_T = d$phi_T, phi_E = d$phi_E,
                             phi1 = d$phi1, phi2 = d$phi2), u)
  })
  
  truth_df <- reactive({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    tr <- truth()
    tb <- tr$table
    if (boin_is_pk_method(boin_rv$overall_setting$method)) {
      D <- boin_rv$trial_setting$n_dose
      r <- boin_rv$scenario_setting$r_true
      pk <- boin_rv$pk_setting
      req(length(r) == D, pk$r_P, pk$r_I_mult)
      zeta1 <- pkboin_zeta1(pk$r_P, pk$r_I_mult)
      d_pk_true <- which.min(abs(r - pk$r_P))
      b <- boin_boundaries(boin_rv$design_setting$phi_T,
                           phi1 = boin_rv$design_setting$phi1,
                           phi2 = boin_rv$design_setting$phi2)
      u <- c(boin_rv$design_setting$u1, boin_rv$design_setting$u2,
             boin_rv$design_setting$u3, boin_rv$design_setting$u4)
      obd <- pkboin_true_obd(
        boin_rv$scenario_setting$p_true,
        boin_rv$scenario_setting$q_true,
        r,
        list(phi_T = boin_rv$design_setting$phi_T,
             phi_E = boin_rv$design_setting$phi_E),
        b, u, list(r_P = pk$r_P)
      )
      tb$r_true <- round(r, 1)
      tb$pk_adequate <- ifelse(r > zeta1, "yes", "no")
      tb$is_PK_min <- ifelse(seq_len(D) == d_pk_true, "x", "")
      tb$is_OBD <- ifelse(!is.na(obd) & seq_len(D) == obd, "*", "")
      tb <- tb[, c("dose", "p_true", "q_true", "r_true", "pi1", "pi2", "pi3", "pi4",
                   "EU_d", "RDS_true", "pk_adequate", "is_PK_min", "is_MTD", "is_OBD")]
    }
    tb
  })
  
  output$truth_table <- DT::renderDT({
    tb <- truth_df()
    is_pk <- boin_is_pk_method(boin_rv$overall_setting$method)
    editable_cols <- if (is_pk) c(1, 2, 3) else c(1, 2)
    disabled_cols <- setdiff(seq_along(names(tb)) - 1, editable_cols)
    DT::datatable(
      tb,
      rownames = FALSE,
      selection = "none",
      editable = list(target = "cell", disable = list(columns = disabled_cols)),
      options = list(dom = "t", ordering = FALSE, scrollX = TRUE,
                     destroy = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)
  
  observeEvent(input$truth_table_cell_edit, {
    req(boin_method_implemented(boin_rv$overall_setting$method))
    info <- input$truth_table_cell_edit
    D <- boin_rv$trial_setting$n_dose
    val <- suppressWarnings(as.numeric(info$value))
    req(!is.na(val), info$row >= 1, info$row <= D)
    if (info$col == 1) boin_rv$scenario_setting$p_true[info$row] <- val
    if (info$col == 2) boin_rv$scenario_setting$q_true[info$row] <- val
    if (boin_is_pk_method(boin_rv$overall_setting$method) && info$col == 3) {
      boin_rv$scenario_setting$r_true[info$row] <- val
    }
  }, ignoreInit = TRUE)
  
  output$truth_plot <- renderPlot({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    tr <- truth()
    tb <- tr$table
    D <- nrow(tb)
    
    if (boin_is_pk_method(boin_rv$overall_setting$method)) {
      r <- boin_rv$scenario_setting$r_true
      pk <- boin_rv$pk_setting
      req(length(r) == D, pk$r_P, pk$r_I_mult)
      zeta1 <- pkboin_zeta1(pk$r_P, pk$r_I_mult)
      pk_axis_max <- max(r, zeta1, pk$r_P, na.rm = TRUE)
      d_pk_true <- which.min(abs(r - pk$r_P))
      b <- boin_boundaries(boin_rv$design_setting$phi_T,
                           phi1 = boin_rv$design_setting$phi1,
                           phi2 = boin_rv$design_setting$phi2)
      u <- c(boin_rv$design_setting$u1, boin_rv$design_setting$u2,
             boin_rv$design_setting$u3, boin_rv$design_setting$u4)
      obd <- pkboin_true_obd(
        boin_rv$scenario_setting$p_true,
        boin_rv$scenario_setting$q_true,
        r,
        list(phi_T = boin_rv$design_setting$phi_T,
             phi_E = boin_rv$design_setting$phi_E),
        b, u, list(r_P = pk$r_P)
      )
      
      mark <- boin_label_stack(
        dose = c(tr$d_mtd, obd, d_pk_true),
        label = c("MTD", if (!is.na(obd)) "OBD" else NA_character_, "PK-min")
      )
      
      df <- data.frame(
        dose = rep(tb$dose, 3),
        value = c(tb$p_true, tb$q_true, r / pk_axis_max),
        type = rep(c("toxicity", "efficacy", "PK mean"), each = D)
      )
      
      ggplot2::ggplot(df, ggplot2::aes(dose, value, colour = type, linetype = type)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::geom_hline(yintercept = zeta1 / pk_axis_max, linetype = "dotted", colour = "#A05A00") +
        ggplot2::geom_label(
          data = mark,
          ggplot2::aes(dose, y, label = label),
          inherit.aes = FALSE,
          size = 3.8,
          label.size = 0.2,
          fill = "white"
        ) +
        ggplot2::scale_y_continuous(
          name = "True probability", limits = c(0, 1),
          sec.axis = ggplot2::sec_axis(~ . * pk_axis_max, name = "True mean PK")
        ) +
        ggplot2::scale_colour_manual(values = c(toxicity = "#E24B4A", efficacy = "#1D9E75", `PK mean` = "#A05A00")) +
        ggplot2::scale_linetype_manual(values = c(toxicity = "dashed", efficacy = "dashed", `PK mean` = "solid")) +
        ggplot2::labs(x = "Dose level", colour = NULL, linetype = NULL,
                      title = "True toxicity / efficacy / PK curves") +
        ggplot2::theme_minimal(base_size = 15)
    } else {
      mark <- boin_label_stack(
        dose = c(tr$d_mtd, tr$obd),
        label = c("MTD", if (!is.na(tr$obd)) "OBD" else NA_character_)
      )
      
      df <- data.frame(
        dose = rep(tb$dose, 3),
        value = c(tb$p_true, tb$q_true, tb$EU_d / 100),
        type = rep(c("toxicity", "efficacy", "utility (EU_d)"), each = D)
      )
      
      ggplot2::ggplot(df, ggplot2::aes(dose, value, colour = type, linetype = type)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::geom_hline(yintercept = tr$u_b / 100, linetype = "dotted", colour = "#888") +
        ggplot2::geom_label(
          data = mark,
          ggplot2::aes(dose, y, label = label),
          inherit.aes = FALSE,
          size = 3.8,
          label.size = 0.2,
          fill = "white"
        ) +
        ggplot2::scale_y_continuous(
          name = "True probability", limits = c(0, 1),
          sec.axis = ggplot2::sec_axis(~ . * 100, name = "Expected utility EU_d")
        ) +
        ggplot2::scale_colour_manual(values = c(toxicity = "#E24B4A", efficacy = "#1D9E75",
                                                `utility (EU_d)` = "#7F77DD")) +
        ggplot2::scale_linetype_manual(values = c(toxicity = "dashed", efficacy = "dashed",
                                                  `utility (EU_d)` = "solid")) +
        ggplot2::labs(x = "Dose level", colour = NULL, linetype = NULL,
                      title = "True dose-toxicity / dose-efficacy / dose-utility") +
        ggplot2::theme_minimal(base_size = 15)
    }
  })
}
