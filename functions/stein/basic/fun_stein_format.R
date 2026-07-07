# =====================================================================
# Shared numeric display formatter (pure function; display-layer only)
# ---------------------------------------------------------------------
# Used wherever a table needs to show a computed probability/utility
# value to the user. Underlying computation functions (fun_stein_*.R)
# keep returning full-precision doubles -- this only formats values
# for display, at the point they're written into a table/plot:
#   - 0 stays "0.000"
#   - |x| >= 1e5, or (x != 0 and |x| < 1e-4) -> scientific notation
#     with `digits` significant decimal places (e.g. "1.234e-05")
#   - otherwise -> fixed notation rounded to `digits` decimal places
# Returns a character vector (safe for direct display in a DT table).
# =====================================================================

stein_fmt_num <- function(x, digits = 3) {
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
