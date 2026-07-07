# =====================================================================
# STEIN upload validation (pure function; upload mode only)
# ---------------------------------------------------------------------
# Detects cohort-level (cohort, dose, n, n_dlt, n_eff) or patient-level
# (patient_id, cohort, dose, dlt, response) format, aggregates
# patient-level to cohort-level, and validates against n_dose.
#
# Anything that would break downstream computation is BLOCKING
# (ok = FALSE, nothing imported): missing required columns, non-numeric
# or missing values, non-integer values, dose outside 1..n_dose,
# negative n, negative n_dlt/n_eff, or n_dlt/n_eff exceeding n (the
# last of these would make 1+n-n_dlt negative, an invalid Beta shape
# parameter downstream in stein_prob_tox_above/stein_prob_eff_*).
#
# Rows with n = 0 are NOT an error: they are untried-dose placeholders
# (e.g. in a final per-dose summary format where every dose gets a row
# even if never enrolled) and are silently dropped after validation --
# they carry no enrollment data, so they are not real cohorts.
#
# Missing/duplicate cohort ordering does NOT break computation (rows
# can still be ordered), so it is a non-blocking warning: falls back
# to file row order and the import proceeds.
# =====================================================================

stein_validate_upload <- function(raw, n_dose) {
  warnings <- character(0)

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, data = NULL,
                errors = "Uploaded file is empty or could not be read.",
                warnings = character(0)))
  }
  cols <- colnames(raw)

  is_cohort_fmt  <- all(c("dose", "n", "n_dlt", "n_eff") %in% cols)
  is_patient_fmt <- all(c("dose", "dlt", "response") %in% cols)

  if (!is_cohort_fmt && !is_patient_fmt) {
    return(list(ok = FALSE, data = NULL,
                errors = "File matches neither required format: cohort-level (cohort, dose, n, n_dlt, n_eff) or patient-level (patient_id, cohort, dose, dlt, response).",
                warnings = character(0)))
  }

  if (is_patient_fmt) {
    if (!"cohort" %in% cols) {
      return(list(ok = FALSE, data = NULL,
                  errors = "Patient-level file is missing the 'cohort' column, required to aggregate to cohort-level and order the trial.",
                  warnings = character(0)))
    }
    agg <- stats::aggregate(
      cbind(n = rep(1, nrow(raw)), n_dlt = raw$dlt, n_eff = raw$response) ~ cohort + dose,
      data = raw, FUN = sum
    )
    df <- agg[, c("cohort", "dose", "n", "n_dlt", "n_eff")]
  } else {
    df <- raw[, intersect(c("cohort", "dose", "n", "n_dlt", "n_eff"), cols), drop = FALSE]
    if (!"cohort" %in% colnames(df)) {
      df$cohort <- seq_len(nrow(df))
      warnings <- c(warnings, "No 'cohort' column found; using file row order as enrollment order.")
    }
  }

  # cohort must be usable for ordering; fall back to row order if not (non-blocking)
  cohort_num <- suppressWarnings(as.numeric(df$cohort))
  if (any(is.na(cohort_num))) {
    df$cohort <- seq_len(nrow(df))
    warnings <- c(warnings, "'cohort' column contains non-numeric values; using file row order as enrollment order instead.")
  } else {
    df$cohort <- cohort_num
  }

  # ---- numeric / missing-value checks (blocking) ----
  errors <- character(0)
  num_cols <- c("dose", "n", "n_dlt", "n_eff")
  for (cc in num_cols) {
    v <- suppressWarnings(as.numeric(df[[cc]]))
    if (any(is.na(v))) {
      errors <- c(errors, sprintf("Column '%s' contains non-numeric or missing values.", cc))
    } else {
      df[[cc]] <- v
    }
  }
  if (length(errors) > 0) {
    return(list(ok = FALSE, data = NULL, errors = errors, warnings = warnings))
  }

  # ---- integer checks (blocking) ----
  if (any(df$dose != round(df$dose)) || any(df$n != round(df$n)) ||
      any(df$n_dlt != round(df$n_dlt)) || any(df$n_eff != round(df$n_eff))) {
    errors <- c(errors, "dose, n, n_dlt, and n_eff must all be whole numbers.")
  }

  # ---- range / consistency checks (blocking -- these break downstream computation) ----
  if (any(df$dose < 1 | df$dose > n_dose)) {
    errors <- c(errors, sprintf("dose values must be between 1 and %d (the configured number of doses); found values outside this range.", n_dose))
  }
  if (any(df$n < 0)) {
    errors <- c(errors, "n (cohort size) cannot be negative.")
  }
  if (any(df$n_dlt < 0) || any(df$n_eff < 0)) {
    errors <- c(errors, "n_dlt and n_eff cannot be negative.")
  }
  if (any(df$n_dlt > df$n) || any(df$n_eff > df$n)) {
    errors <- c(errors, "n_dlt and n_eff cannot exceed n (cohort size) in any row -- this would make the posterior toxicity/efficacy calculations invalid.")
  }

  if (length(errors) > 0) {
    return(list(ok = FALSE, data = NULL, errors = errors, warnings = warnings))
  }

  # n = 0 rows carry no enrollment (e.g. an untried-dose placeholder row in a
  # final per-dose summary format) -- valid, but not a real cohort, so they
  # are dropped rather than kept as a phantom "cohort" in the replay log.
  n_dropped <- sum(df$n == 0)
  df <- df[df$n > 0, , drop = FALSE]
  if (nrow(df) == 0) {
    return(list(ok = FALSE, data = NULL,
                errors = "No cohorts with n > 0 found -- every row is an untried-dose placeholder (n = 0). Nothing to analyze.",
                warnings = warnings))
  }
  if (n_dropped > 0) {
    warnings <- c(warnings, sprintf("%d row(s) with n = 0 (untried-dose placeholders) were ignored -- they carry no enrollment data.", n_dropped))
  }

  # ---- non-blocking: duplicate cohort numbers ----
  if (any(duplicated(df$cohort))) {
    warnings <- c(warnings, "Duplicate cohort numbers found; rows are ordered by cohort value, ties broken by file row order.")
  }

  df <- df[order(df$cohort), c("cohort", "dose", "n", "n_dlt", "n_eff")]
  df$dose  <- as.integer(df$dose)
  df$n     <- as.integer(df$n)
  df$n_dlt <- as.integer(df$n_dlt)
  df$n_eff <- as.integer(df$n_eff)
  rownames(df) <- NULL

  list(ok = TRUE, data = df, errors = character(0), warnings = warnings)
}
