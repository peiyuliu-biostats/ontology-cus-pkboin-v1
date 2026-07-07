# =====================================================================
# STEIN Flowchart tab module (new, standalone -- not nested in Design)
# ---------------------------------------------------------------------
# Static decision-flow diagram (dose-assignment rule + elimination +
# stopping + final OBD selection) plus a boundary look-up table, in the
# style of the STEIN paper's Table 1. Read-only; no inputs. Drawn with
# base graphics to avoid adding a flowchart-drawing dependency.
# =====================================================================

module_UI_stein_flowchart <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Dose-assignment decision flow"),
    plotOutput(ns("flow_plot"), height = "560px"),
    tags$h4("Boundary look-up"),
    DT::DTOutput(ns("lookup_table"))
  )
}

module_server_stein_flowchart <- function(input, output, session, stein_rv) {

  bounds <- reactive({
    d <- stein_rv$design_setting
    req(d$phi0, d$psi1, d$psi2, d$phi1, d$phi2)
    stein_boundaries(d$phi0, d$psi1, d$psi2, phi1 = d$phi1, phi2 = d$phi2)
  })

  output$flow_plot <- renderPlot({
    b <- bounds()
    d <- stein_rv$design_setting

    draw_box <- function(x, y, w, h, label, fill = "#F2F2F7", cex = 0.82) {
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
    plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 13),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")

    draw_box(5, 12.3, 4.6, 0.9, "Enroll cohort at current dose j")
    draw_arrow(5, 11.85, 5, 11.05)

    draw_box(5, 10.6, 6.2, 0.9,
             "Observe cohort toxicity p\u0302 and efficacy q\u0302; update cumulative counts")
    draw_arrow(5, 10.15, 5, 9.35)

    draw_box(2.5, 8.6, 4.6, 1.6,
             sprintf("Dose-transition rule\np\u0302 \u2264 %.3f \u2192 escalate\n%.3f < p\u0302 < %.3f \u2192 stay\np\u0302 \u2265 %.3f \u2192 de-escalate",
                     b$phiL, b$phiL, b$phiU, b$phiU),
             fill = "#FDEBD3", cex = 0.78)
    draw_box(7.5, 8.6, 4.6, 1.6,
             sprintf("Elimination rule\nPr(p > \u03c60 | data) > %.2f \u2192 eliminate dose\nPr(q \u2264 \u03c81 | data) > %.2f \u2192 eliminate dose",
                     d$CT, d$CE),
             fill = "#F7C1C1", cex = 0.78)

    draw_arrow(2.5, 7.8, 5, 6.75)
    draw_arrow(7.5, 7.8, 5, 6.75)

    draw_box(5, 6.3, 7.0, 1.0,
             "Lowest dose eliminated, or all tried doses eliminated?",
             fill = "#FBE9E7", cex = 0.78)

    draw_arrow(3.6, 5.8, 2.3, 4.6, "yes")
    draw_arrow(6.4, 5.8, 7.7, 4.6, "no")

    draw_box(2.3, 4.1, 4.0, 1.0, "Stop trial for safety/futility\n(no OBD selected)",
             fill = "#F7C1C1", cex = 0.76)
    draw_box(7.7, 4.1, 4.0, 1.0, "Cumulative n reached N_max?",
             fill = "#F2F2F7", cex = 0.78)

    draw_arrow(6.7, 3.6, 3.3, 1.9)
    text(5, 2.9, "no: enroll next cohort\nat updated dose", cex = 0.72, col = "#444444")

    draw_arrow(7.7, 3.6, 7.7, 2.1, "yes")

    draw_box(5, 1.3, 7.4, 1.3,
             "Final OBD selection: isotonic toxicity (PAVA) +\nunimodal efficacy (AIC model averaging) \u2192 argmax utility\nU = q\u0303 \u2212 w1\u00b7p\u0303 \u2212 w2\u00b7p\u0303\u00b7I(p\u0303 > \u03c60), non-eliminated doses only",
             fill = "#D6EAF8", cex = 0.74)
  }, res = 108)

  output$lookup_table <- DT::renderDT({
    b <- bounds()
    df <- data.frame(
      Quantity = c("\u03c61 (lower toxicity anchor)",
                   "\u03c62 (upper toxicity anchor)",
                   "\u03c6L (escalate boundary)",
                   "\u03c6U (de-escalate boundary)",
                   "\u03c8 (efficacy cutoff)"),
      Value = sprintf("%.4f", c(b$phi1, b$phi2, b$phiL, b$phiU, b$psi))
    )
    DT::datatable(df, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
}
