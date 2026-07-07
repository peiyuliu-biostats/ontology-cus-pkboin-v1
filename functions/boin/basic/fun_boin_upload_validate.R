# =====================================================================
# BOIN-12 upload validation (pure function; upload mode only)
# ---------------------------------------------------------------------
# Detects cohort-level (cohort, dose, n, n1, n2, n3, n4 -- the joint
# 2x2 toxicity x efficacy category counts, see
# functions/boin/data/initial_boin_rv.R for the encoding) or
# patient-level (patient_id, cohort, dose, dlt, response) format;
# patient-level rows are classified into the 4 joint categories from
# their own (dlt, response) pair and aggregated to cohort-level.
# Same blocking/non-blocking error convention as
# functions/stein/basic/fun_stein_upload_validate.R (stein_validate_upload),
# duplicated rather than shared so functions/boin/ has no dependency on
# functions/stein/.
# =====================================================================

boin_validate_upload <- function(raw, n_dose) {
  warnings <- character(0)

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, data = NULL,
                errors = "Uploaded file is empty or could not be read.",
                warnings = character(0)))
  }
  cols <- colnames(raw)

  is_cohort_fmt  <- all(c("dose", "n", "n1", "n2", "n3", "n4") %in% cols)
  is_patient_fmt <- all(c("dose", "dlt", "response") %in% cols)

  if (!is_cohort_fmt && !is_patient_fmt) {
    return(list(ok = FALSE, data = NULL,
                errors = "File matches neither required format: cohort-level (cohort, dose, n, n1, n2, n3, n4) or patient-level (patient_id, cohort, dose, dlt, response).",
                warnings = character(0)))
  }

  if (is_patient_fmt) {
    if (!"cohort" %in% cols) {
      return(list(ok = FALSE, data = NULL,
                  errors = "Patient-level file is missing the 'cohort' column, required to aggregate to cohort-level and order the trial.",
                  warnings = character(0)))
    }
    # classify each patient into one of the 4 joint categories from
    # their own (dlt, response): 1=eff&notox, 2=eff&tox, 3=noeff&notox, 4=noeff&tox
    dlt <- suppressWarnings(as.numeric(raw$dlt))
    resp <- suppressWarnings(as.numeric(raw$response))
    if (any(is.na(dlt)) || any(is.na(resp)) ||
        !all(dlt %in% c(0, 1)) || !all(resp %in% c(0, 1))) {
      return(list(ok = FALSE, data = NULL,
                  errors = "Columns 'dlt' and 'response' must each be 0/1 for every patient row.",
                  warnings = character(0)))
    }
    raw$.n1 <- as.integer(resp == 1 & dlt == 0)
    raw$.n2 <- as.integer(resp == 1 & dlt == 1)
    raw$.n3 <- as.integer(resp == 0 & dlt == 0)
    raw$.n4 <- as.integer(resp == 0 & dlt == 1)
    agg <- stats::aggregate(
      cbind(n = rep(1, nrow(raw)), n1 = raw$.n1, n2 = raw$.n2, n3 = raw$.n3, n4 = raw$.n4) ~ cohort + dose,
      data = raw, FUN = sum
    )
    df <- agg[, c("cohort", "dose", "n", "n1", "n2", "n3", "n4")]
  } else {
    df <- raw[, intersect(c("cohort", "dose", "n", "n1", "n2", "n3", "n4"), cols), drop = FALSE]
    if (!"cohort" %in% colnames(df)) {
      df$cohort <- seq_len(nrow(df))
      warnings <- c(warnings, "No 'cohort' column found; using file row order as enrollment order.")
    }
  }

  cohort_num <- suppressWarnings(as.numeric(df$cohort))
  if (any(is.na(cohort_num))) {
    df$cohort <- seq_len(nrow(df))
    warnings <- c(warnings, "'cohort' column contains non-numeric values; using file row order as enrollment order instead.")
  } else {
    df$cohort <- cohort_num
  }

  # ---- numeric / missing-value checks (blocking) ----
  errors <- character(0)
  num_cols <- c("dose", "n", "n1", "n2", "n3", "n4")
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
      any(df$n1 != round(df$n1)) || any(df$n2 != round(df$n2)) ||
      any(df$n3 != round(df$n3)) || any(df$n4 != round(df$n4))) {
    errors <- c(errors, "dose, n, n1, n2, n3, and n4 must all be whole numbers.")
  }

  # ---- range / consistency checks (blocking) ----
  if (any(df$dose < 1 | df$dose > n_dose)) {
    errors <- c(errors, sprintf("dose values must be between 1 and %d (the configured number of doses); found values outside this range.", n_dose))
  }
  if (any(df$n < 0) || any(df$n1 < 0) || any(df$n2 < 0) || any(df$n3 < 0) || any(df$n4 < 0)) {
    errors <- c(errors, "n, n1, n2, n3, and n4 cannot be negative.")
  }
  if (any(df$n1 + df$n2 + df$n3 + df$n4 != df$n)) {
    errors <- c(errors, "n1 + n2 + n3 + n4 must equal n in every row -- the four joint categories must exactly partition the cohort.")
  }

  if (length(errors) > 0) {
    return(list(ok = FALSE, data = NULL, errors = errors, warnings = warnings))
  }

  # n = 0 rows carry no enrollment (untried-dose placeholder rows) -- valid,
  # but not a real cohort, so they are dropped rather than kept as a
  # phantom "cohort" in the replay log.
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

  if (any(duplicated(df$cohort))) {
    warnings <- c(warnings, "Duplicate cohort numbers found; rows are ordered by cohort value, ties broken by file row order.")
  }

  df <- df[order(df$cohort), c("cohort", "dose", "n", "n1", "n2", "n3", "n4")]
  df$dose <- as.integer(df$dose)
  df$n    <- as.integer(df$n)
  df$n1   <- as.integer(df$n1)
  df$n2   <- as.integer(df$n2)
  df$n3   <- as.integer(df$n3)
  df$n4   <- as.integer(df$n4)
  rownames(df) <- NULL

  list(ok = TRUE, data = df, errors = character(0), warnings = warnings)
}
