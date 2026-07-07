# =====================================================================
# Shared numeric display formatter for BOIN tabs (display-layer only)
# ---------------------------------------------------------------------
# Same convention as functions/stein/basic/fun_stein_format.R
# (stein_fmt_num), duplicated under its own name so functions/boin/ has
# no source-order/edit dependency on functions/stein/.
# =====================================================================

boin_fmt_num <- function(x, digits = 3) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v == 0) return(formatC(0, format = "f", digits = digits))
    if (abs(v) >= 1e5 || abs(v) < 1e-4) {
      formatC(v, format = "e", digits = digits)
    } else {
      formatC(v, format = "f", digits = digits)
    }
  }, character(1))
}
