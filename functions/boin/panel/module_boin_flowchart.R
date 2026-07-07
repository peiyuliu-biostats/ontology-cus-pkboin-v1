# =====================================================================
# BOIN-12 Flowchart tab module
# ---------------------------------------------------------------------
# Static decision-flow diagram (toxicity guardrail + utility comparison
# + elimination + stopping + final OBD selection), mirroring
# functions/stein/panel/module_stein_flowchart.R's layout/base-graphics
# approach, adapted to BOIN-12's own rule. Read-only; no inputs.
# =====================================================================

module_UI_boin_flowchart <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stage_notice")),
    tags$h4("Dose-assignment decision flow"),
    plotOutput(ns("flow_plot"), height = "600px"),
    tags$h4("Boundary look-up"),
    DT::DTOutput(ns("lookup_table"))
  )
}

module_server_boin_flowchart <- function(input, output, session, boin_rv) {
  
  output$stage_notice <- renderUI(boin_stage_notice_ui(boin_rv$overall_setting$method))
  
  bounds <- reactive({
    d <- boin_rv$design_setting
    req(d$phi_T)
    boin_boundaries(d$phi_T, phi1 = d$phi1, phi2 = d$phi2)
  })
  
  output$flow_plot <- renderPlot({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    b <- bounds()
    d <- boin_rv$design_setting
    method <- boin_rv$overall_setting$method
    is_pk <- boin_is_pk_method(method)
    is_tite <- identical(method, "TITE-PKBOIN-12")
    
    draw_box <- function(x, y, w, h, label, fill = "#F2F2F7", cex = 0.8) {
      rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2,
           col = fill, border = "#999999", lwd = 1.2)
      text(x, y, label, cex = cex, adj = c(0.5, 0.5))
    }
    draw_arrow <- function(x0, y0, x1, y1, label = NULL, cex = 0.72) {
      arrows(x0, y0, x1, y1, length = 0.09, col = "#666666", lwd = 1.2)
      if (!is.null(label)) {
        text((x0 + x1) / 2, (y0 + y1) / 2 + 0.22, label, cex = cex, col = "#444444")
      }
    }
    
    par(mar = c(0.5, 0.5, 0.5, 0.5))
    plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 14),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    
    # -----------------------------------------------------------------
    # TITE-PKBOIN-12 flowchart: drawn on its OWN vertical layout so the
    # two extra boxes (decision-time observation + AL imputation) never
    # overlap the PK branch / elimination row. Independent of the PKBOIN
    # branch below, which stays unchanged.
    # -----------------------------------------------------------------
    if (is_tite) {
      pk <- boin_rv$pk_setting
      z  <- pkboin_zeta1(pk$r_P, pk$r_I_mult)

      draw_box(5, 13.35, 5.6, 0.7, "Enroll cohort at current dose j", cex = 0.72)
      draw_arrow(5, 13.0, 5, 12.55)
      draw_box(5, 12.15, 5.8, 0.8,
               "At decision time t: observe PK and\navailable tox/eff; identify pending outcomes",
               cex = 0.64)
      draw_arrow(5, 11.75, 5, 11.4)
      draw_box(5, 10.95, 5.8, 0.8,
               "AL imputation for pending tox/eff \u2192\nquasi-counts n*_1..n*_4, p*_d, q*_d",
               fill = "#E8DAEF", cex = 0.64)
      # TITE-only accrual-suspension note, parked in the right margin so it
      # overlaps neither the main column nor the plot edge; a short arrow
      # links it to the decision-time row.
      draw_box(8.9, 11.55, 2.0, 1.1,
               "TITE only:\nif >50% pending\nat dose j, suspend\naccrual for\nmore data",
               fill = "#FADBD8", cex = 0.55)
      draw_arrow(7.9, 11.55, 7.05, 11.55)
      draw_arrow(5, 10.55, 5, 10.15)

      draw_box(5, 9.65, 6.6, 0.8,
               sprintf("PK exposure at dose j: is r\u0302_d > \u03b61 = %.0f ?", z),
               fill = "#FFF0CC", cex = 0.72)
      draw_arrow(2.6, 9.25, 2.6, 8.85, "no (r\u0302_d \u2264 \u03b61)")
      draw_arrow(7.4, 9.25, 7.4, 8.85, "yes (r\u0302_d > \u03b61)")

      draw_box(2.5, 8.05, 4.5, 1.4,
               sprintf("TITE quasi-count BOIN12 rule:\ntox rule over {j-1,j,j+1}\nusing \u03bbe=%.3f, \u03bbd=%.3f\npick max RDS",
                       b$lambda_e, b$lambda_d),
               fill = "#FDEBD3", cex = 0.64)
      draw_box(7.5, 8.05, 4.5, 1.4,
               "Expanded set: lower end\ndrops to d* = min(j-1, d_PK,min)\ntox rule over {d*,...,j+1}\npick max RDS",
               fill = "#FBE3B8", cex = 0.64)

      draw_arrow(2.5, 7.35, 5, 6.95)
      draw_arrow(7.5, 7.35, 5, 6.95)

      draw_box(5, 6.15, 8.6, 1.5,
               sprintf("Elimination\nTox: Pr(tox>\u03c6T|p*_d)>%.2f \u2192 j & above (cascade)\nEff: Pr(eff\u2264\u03c6E|q*_d)>%.2f \u2192 j (futility)\nPK: n\u22656 & Pr(r_d<r_P|.)>%.2f \u2192 prune low dose; if dose D \u2192 terminate",
                       d$CT, d$CE, pk$C_P),
               fill = "#F7C1C1", cex = 0.62)
      draw_arrow(5, 5.4, 5, 5.05)

      draw_box(5, 4.55, 7.4, 0.9,
               "Lowest dose eliminated / all doses eliminated / PK-terminated?",
               fill = "#FBE9E7", cex = 0.68)
      draw_arrow(3.3, 4.1, 2.2, 3.55, "yes")
      draw_arrow(6.7, 4.1, 7.8, 3.55, "no")
      draw_box(2.2, 3.05, 4.0, 0.9, "Stop trial\n(no OBD selected)", fill = "#F7C1C1", cex = 0.68)
      draw_box(7.8, 3.05, 4.0, 0.9, "Cumulative n reached N_max?", fill = "#F2F2F7", cex = 0.68)
      draw_arrow(6.9, 2.6, 3.4, 1.75)
      text(4.9, 2.3, "no: next cohort", cex = 0.62, col = "#444444")
      draw_arrow(7.8, 2.6, 7.8, 1.7, "yes")
      draw_box(5, 1.15, 8.8, 1.0,
               "Final OBD (3 steps): isotonic tox \u2192 d_MTD; isotonic PK \u2192 d*_PK,min;\nmax utility among {d*_PK,min..d_MTD} not eliminated",
               fill = "#D6EAF8", cex = 0.62)
      return(invisible(NULL))
    }

    if (is_pk) {
      pk <- boin_rv$pk_setting
      z  <- pkboin_zeta1(pk$r_P, pk$r_I_mult)

      draw_box(5, 13.3, 5.2, 0.9, "Enroll cohort at current dose j")
      draw_arrow(5, 12.85, 5, 12.05)
      draw_box(5, 11.6, 7.2, 0.9,
               "Observe joint eff/tox outcomes AND PK; update n1..n4 and PK mean r\u0302_d")
      draw_arrow(5, 11.15, 5, 10.5)

      # PK branch first (this is the PKBOIN-12-specific wrapper)
      draw_box(5, 10.0, 6.6, 0.9,
               sprintf("PK exposure at dose j: is r\u0302_d > \u03b61 = %.0f ?", z),
               fill = "#FFF0CC", cex = 0.78)
      draw_arrow(2.6, 9.55, 2.6, 8.75, "no (r\u0302_d \u2264 \u03b61)")
      draw_arrow(7.4, 9.55, 7.4, 8.75, "yes (r\u0302_d > \u03b61)")

      draw_box(2.6, 8.25, 4.4, 1.5,
               sprintf("Same as BOIN12:\ntox rule over {j-1,j,j+1}\nusing \u03bbe=%.3f, \u03bbd=%.3f\npick max RDS",
                       b$lambda_e, b$lambda_d),
               fill = "#FDEBD3", cex = 0.68)
      draw_box(7.4, 8.25, 4.6, 1.5,
               "Expanded set: lower end\ndrops to d* = min(j-1, d_PK,min)\ntox rule over {d*,...,j+1}\npick max RDS",
               fill = "#FBE3B8", cex = 0.68)

      draw_arrow(2.6, 7.5, 5, 6.55)
      draw_arrow(7.4, 7.5, 5, 6.55)

      draw_box(5, 6.05, 7.8, 1.2,
               sprintf("Elimination\nTox: Pr(tox>\u03c6T|.)>%.2f \u2192 j & above (cascade)\nEff: Pr(eff\u2264\u03c6E|.)>%.2f \u2192 j (futility)\nPK: n\u22656 & Pr(r_d<r_P|.)>%.2f \u2192 prune low dose; if dose D \u2192 terminate",
                       d$CT, d$CE, pk$C_P),
               fill = "#F7C1C1", cex = 0.66)
      draw_arrow(5, 5.45, 5, 4.55)

      draw_box(5, 4.05, 7.0, 1.0,
               "Lowest dose eliminated / all doses eliminated / PK-terminated?",
               fill = "#FBE9E7", cex = 0.72)
      draw_arrow(3.4, 3.55, 2.2, 2.55, "yes")
      draw_arrow(6.6, 3.55, 7.8, 2.55, "no")
      draw_box(2.2, 2.05, 4.0, 1.0, "Stop trial\n(no OBD selected)", fill = "#F7C1C1", cex = 0.72)
      draw_box(7.8, 2.05, 4.0, 1.0, "Cumulative n reached N_max?", fill = "#F2F2F7", cex = 0.74)
      draw_arrow(6.8, 1.55, 3.3, 0.95)
      text(4.8, 1.35, "no: next cohort", cex = 0.66, col = "#444444")
      draw_arrow(7.8, 1.55, 7.8, 0.85, "yes")
      draw_box(5, 0.4, 8.4, 0.95,
               "Final OBD (3 steps): isotonic tox \u2192 d_MTD; isotonic PK \u2192 d*_PK,min;\nmax utility among {d*_PK,min..d_MTD} not eliminated",
               fill = "#D6EAF8", cex = 0.66)
      return(invisible(NULL))
    }
    
    draw_box(5, 13.3, 4.6, 0.9, "Enroll cohort at current dose j")
    draw_arrow(5, 12.85, 5, 12.05)
    
    draw_box(5, 11.6, 6.6, 0.9,
             "Observe cohort joint outcomes (eff/tox); update cumulative counts n1..n4")
    draw_arrow(5, 11.15, 5, 10.35)
    
    draw_box(2.5, 9.6, 4.8, 1.9,
             sprintf("Toxicity + RDS rule\np\u0302tox \u2265 %.3f \u2192 de-escalate {j-1}\n%.3f<p\u0302tox<%.3f \u2192 {j-1,j} if n_j\u2265N*(6)\n   else {j-1,j,j+1}\np\u0302tox \u2264 %.3f \u2192 {j-1,j,j+1};\n   n_j\u22659 & j+1 unused \u2192 jump to j+1\npick max RDS in the set",
                     b$lambda_d, b$lambda_e, b$lambda_d, b$lambda_e),
             fill = "#FDEBD3", cex = 0.7)
    draw_box(7.5, 9.6, 4.6, 1.9,
             sprintf("Elimination rule\nPr(tox > \u03c6T | data) > %.2f\n   \u2192 eliminate j AND all above (cascade)\nPr(eff \u2264 \u03c6E | data) > %.2f\n   \u2192 eliminate j (futility)",
                     d$CT, d$CE),
             fill = "#F7C1C1", cex = 0.7)
    
    draw_arrow(2.5, 8.8, 5, 7.75)
    draw_arrow(7.5, 8.8, 5, 7.75)
    
    draw_box(5, 7.3, 7.6, 1.0,
             "Within toxicity-admissible, non-eliminated doses: pick the largest RDS = Pr(u_d > u_b | data)",
             fill = "#D6EAF8", cex = 0.74)
    
    draw_arrow(5, 6.8, 5, 5.85)
    
    draw_box(5, 5.4, 7.0, 1.0,
             "Lowest dose eliminated, or all tried doses eliminated?",
             fill = "#FBE9E7", cex = 0.78)
    
    draw_arrow(3.6, 4.9, 2.3, 3.7, "yes")
    draw_arrow(6.4, 4.9, 7.7, 3.7, "no")
    
    draw_box(2.3, 3.2, 4.0, 1.0, "Stop trial for safety/futility\n(no OBD selected)",
             fill = "#F7C1C1", cex = 0.76)
    draw_box(7.7, 3.2, 4.0, 1.0, "Cumulative n reached N_max?",
             fill = "#F2F2F7", cex = 0.78)
    
    draw_arrow(6.7, 2.7, 3.3, 1.0)
    text(5, 2.0, "no: enroll next cohort\nat updated dose", cex = 0.72, col = "#444444")
    
    draw_arrow(7.7, 2.7, 7.7, 1.2, "yes")
    
    draw_box(5, 0.4, 7.6, 1.0,
             "Final OBD: isotonic toxicity \u2192 d_MTD (argmin|p\u0303-\u03c6T|),\nthen max utility among non-eliminated doses \u2264 d_MTD",
             fill = "#D6EAF8", cex = 0.72)
  }, res = 108)
  
  output$lookup_table <- DT::renderDT({
    req(boin_method_implemented(boin_rv$overall_setting$method))
    b <- bounds()
    d <- boin_rv$design_setting
    u_b <- boin_utility_benchmark(c(d$u1, d$u2, d$u3, d$u4), d$phi_T, d$phi_E)
    quantity <- c("\u03c61 (lower toxicity anchor)",
                  "\u03c62 (upper toxicity anchor)",
                  "\u03bbe (escalate-allowed boundary)",
                  "\u03bbd (de-escalate-only boundary)",
                  "N* (sample-size cutoff)",
                  "u_b (RDS utility benchmark, 0-100)")
    value <- c(sprintf("%.4f", c(b$phi1, b$phi2, b$lambda_e, b$lambda_d)),
               "6", sprintf("%.2f", u_b))
    if (boin_is_pk_method(boin_rv$overall_setting$method)) {
      pk <- boin_rv$pk_setting
      req(pk$r_P, pk$r_I_mult, pk$C_P)
      z  <- pkboin_zeta1(pk$r_P, pk$r_I_mult)
      quantity <- c(quantity,
                    "r_P (target PK value)",
                    "r_I (inefficacious PK)",
                    "\u03b61 (PK cutoff = (r_P+r_I)/2)",
                    "C_P (PK elimination cutoff)")
      value <- c(value,
                 sprintf("%.0f", pk$r_P),
                 sprintf("%.0f", pk$r_I_mult * pk$r_P),
                 sprintf("%.0f", z),
                 sprintf("%.2f", pk$C_P))
    }
    if (identical(boin_rv$overall_setting$method, "TITE-PKBOIN-12")) {
      tt <- boin_rv$tite_setting
      quantity <- c(quantity,
                    "A_T (toxicity window)",
                    "A_E (efficacy window)",
                    "accrual interval",
                    "suspension cutoff")
      value <- c(value,
                 sprintf("%.1f", tt$A_T),
                 sprintf("%.1f", tt$A_E),
                 sprintf("%.2f", tt$accrual_rate),
                 sprintf("%.2f", tt$suspend_threshold))
    }
    df <- data.frame(Quantity = quantity, Value = value)
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
}
