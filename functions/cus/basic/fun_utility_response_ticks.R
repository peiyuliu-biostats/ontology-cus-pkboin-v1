# =========================================================
#   Helper: response-axis ticks for a continuous endpoint (display only)
#
#   The utility plot's x is always the rate m in [0,1] and the curve/score are
#   computed on m (CUS core untouched). For a continuous endpoint we only RELABEL
#   the x ticks to the response value that maps to each m.
#
#   simulate mapping is min-max:  m = (Yhat - min(Yhat)) / (max(Yhat) - min(Yhat))
#     inverse:  Yhat(m) = ymin + m * (ymax - ymin)   (model-predicted response)
#
#   upload mapping is the empirical CDF of the OBSERVED Y:  m = ECDF(Yobs)(Yhat)
#     inverse:  response(m) = quantile(Yobs, probs = m)  (true observed response)
#     (the optional response bounds change how Yhat maps to m, but the tick labels
#      reflect the observed-Y quantiles, so the axis stays in real response units.)
#
#   Returns NULL when relabelling is not applicable (not continuous, degenerate
#   curve, or no observed Y) -> caller keeps the default "Rate" axis.
#   When applicable returns list(tickvals, ticktext, axis_title).
# =========================================================

utility_response_ticks <- function(all_rv, group, index) {
  os <- all_rv$overall_setting
  if (is.null(os$simu_or_not)) return(NULL)

  # endpoint must be continuous in the B-scheme type vector, on the right model
  type_vec   <- if (group == "eff") os$eff_type_vec   else os$safe_type_vec
  cont_model <- if (group == "eff") os$eff_cont_model else os$safe_cont_model
  if (is.null(type_vec) || length(type_vec) < index || is.na(type_vec[index]) ||
      type_vec[index] != "cont") return(NULL)
  if (is.null(cont_model) || !(cont_model %in% c(3, 4, 5))) return(NULL)

  tickvals <- seq(0, 1, by = 0.25)

  # ---- UPLOAD: inverse-map m through the observed-Y quantiles ----
  if (os$simu_or_not == 2 && nrow(all_rv$ER_data_list$ER_rawdt) > 0) {
    col <- if (group == "eff") paste0("EFF", index) else paste0("SAFE", index)
    Yobs <- all_rv$ER_data_list$ER_rawdt[[col]]
    Yobs <- Yobs[is.finite(Yobs)]
    if (length(Yobs) == 0) return(NULL)
    ymin <- min(Yobs); ymax <- max(Yobs)
    if (!is.finite(ymin) || !is.finite(ymax) || ymax <= ymin) return(NULL)  # degenerate -> keep Rate
    resp <- as.numeric(quantile(Yobs, probs = tickvals, names = FALSE, type = 7))
    ticktext <- formatC(resp, format = "g", digits = 3)
    return(list(tickvals = tickvals, ticktext = ticktext, axis_title = "Response"))
  }

  # ---- SIMULATE: inverse-map m through the min-max of the predicted curve ----
  if (os$simu_or_not != 1) return(NULL)

  # endpoint's slope / intercept (same store the simulate computation reads)
  es <- if (group == "eff") all_rv$eff_endpoint_setting else all_rv$safe_endpoint_setting
  inter <- es$eff_intercept; slope <- es$eff_slope
  if (group == "safe") { inter <- es$safe_intercept; slope <- es$safe_slope }
  inter <- inter[index]; slope <- slope[index]
  if (is.null(inter) || is.null(slope) || is.na(inter) || is.na(slope)) return(NULL)

  # PK grid over the simulate range
  PKmin <- all_rv$PK_setting$PK_min; PKmax <- all_rv$PK_setting$PK_max
  if (is.null(PKmin) || is.null(PKmax) || is.na(PKmin) || is.na(PKmax) || PKmax <= PKmin) return(NULL)
  PK <- seq(PKmin, PKmax, length.out = 500)

  # predicted response over PK, matching initial_PK_data simulate continuous branch
  Yhat <- switch(as.character(cont_model),
                 "3" = inter + slope * PK,
                 "4" = inter + slope * ifelse(PK > 0, log(PK), NA),
                 "5" = exp(pmin(pmax(inter + slope * PK, -709), 709)))
  ymin <- suppressWarnings(min(Yhat, na.rm = TRUE))
  ymax <- suppressWarnings(max(Yhat, na.rm = TRUE))
  if (!is.finite(ymin) || !is.finite(ymax) || ymax <= ymin) return(NULL)  # flat/degenerate -> keep Rate

  resp <- ymin + tickvals * (ymax - ymin)
  ticktext <- formatC(resp, format = "g", digits = 3)

  list(tickvals = tickvals, ticktext = ticktext,
       axis_title = "Response (model-predicted)")
}
