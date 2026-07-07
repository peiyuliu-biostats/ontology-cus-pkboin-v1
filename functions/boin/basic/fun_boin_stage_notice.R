

# TRUE when the selected method has working computation on this
# (simulate-mode) tab set. Upload-mode / TITE remain deferred.
boin_method_implemented <- function(method) {
  identical(method, "BOIN12") ||
    identical(method, "PKBOIN-12") ||
    identical(method, "TITE-PKBOIN-12")
}

boin_method_label <- function(method) {
  if (identical(method, "PKBOIN-12")) return("PKBOIN12")
  if (identical(method, "TITE-PKBOIN-12")) return("TITE-PKBOIN12")
  method
}

boin_stage_notice_ui <- function(method) {
  if (boin_method_implemented(method)) return(NULL)
  tags$div(
    style = "background:#fdeceb; border-radius:6px; padding:10px 14px; margin-bottom:12px; color:#a32d2d; font-size:13px; font-weight:600;",
    sprintf("%s computation is not implemented yet. Switch Method to BOIN12, PKBOIN12, or TITE-PKBOIN12 simulate mode to see results.",
            boin_method_label(method))
  )
}
