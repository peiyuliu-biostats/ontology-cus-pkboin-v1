# =====================================================================
# TITE-PKBOIN-12 upload-mode replay (pure functions)
# ---------------------------------------------------------------------
# Uploaded patient-level time-to-event data -> deterministic replay.
# Interim tox/eff decisions use TITE approximated-likelihood quasi
# counts; PK rules and final OBD selection reuse PKBOIN-12 primitives.
# =====================================================================

tite_pkboin_validate_upload <- function(raw, n_dose, tite_design = list()) {
  warnings <- character(0)
  td <- tite_pkboin_default_design(tite_design)
  base_required <- c("patient_id", "cohort", "dose", "enroll",
                     "dlt", "response", "pk")

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL, errors = "Uploaded file is empty or could not be read.",
                warnings = character(0)))
  }

  cols <- colnames(raw)
  missing <- setdiff(base_required, cols)
  has_rel_time <- all(c("tox_time", "eff_time") %in% cols)
  has_abs_time <- all(c("tox_event", "eff_event") %in% cols)
  if (length(missing) > 0 || (!has_rel_time && !has_abs_time)) {
    time_msg <- "tox_time and eff_time (days after enrollment), or tox_event and eff_event (absolute event times)"
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL,
                errors = sprintf(
                  "TITE-PKBOIN-12 upload requires patient-level columns: %s, plus %s. Missing: %s.",
                  paste(base_required, collapse = ", "), time_msg,
                  paste(c(missing, if (!has_rel_time && !has_abs_time) time_msg else character(0)),
                        collapse = ", ")
                ),
                warnings = character(0)))
  }

  keep <- unique(c(base_required, intersect(c("tox_time", "eff_time",
                                              "tox_event", "eff_event",
                                              "decision_time"), cols)))
  df <- as.data.frame(raw[, keep, drop = FALSE])
  errors <- character(0)

  num_cols <- setdiff(keep, "patient_id")
  for (cc in num_cols) {
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    if (cc %in% c("tox_time", "eff_time", "tox_event", "eff_event")) next
    if (any(is.na(df[[cc]]))) {
      errors <- c(errors, sprintf("Column '%s' contains missing or non-numeric values.", cc))
    }
  }

  if (length(errors) == 0) {
    if (any(df$cohort != round(df$cohort))) errors <- c(errors, "cohort must be a whole number.")
    if (any(df$dose != round(df$dose))) errors <- c(errors, "dose must be a whole number.")
    if (any(df$dose < 1 | df$dose > n_dose)) {
      errors <- c(errors, sprintf("dose values must be between 1 and %d.", n_dose))
    }
    if (!all(df$dlt %in% c(0, 1))) errors <- c(errors, "dlt must be 0/1 for every patient.")
    if (!all(df$response %in% c(0, 1))) errors <- c(errors, "response must be 0/1 for every patient.")
    if (any(df$pk <= 0)) errors <- c(errors, "pk must be positive for every patient.")
    if (any(df$enroll < 0)) errors <- c(errors, "enroll must be non-negative.")
  }

  if (length(errors) == 0) {
    if (has_abs_time) {
      df$tox_event_abs <- df$tox_event
      df$eff_event_abs <- df$eff_event
      if (any(df$dlt == 1 & is.na(df$tox_event_abs))) {
        errors <- c(errors, "tox_event is required when dlt = 1.")
      }
      if (any(df$response == 1 & is.na(df$eff_event_abs))) {
        errors <- c(errors, "eff_event is required when response = 1.")
      }
    } else {
      if (any(df$dlt == 1 & is.na(df$tox_time))) {
        errors <- c(errors, "tox_time is required when dlt = 1.")
      }
      if (any(df$response == 1 & is.na(df$eff_time))) {
        errors <- c(errors, "eff_time is required when response = 1.")
      }
      df$tox_event_abs <- ifelse(df$dlt == 1, df$enroll + df$tox_time, Inf)
      df$eff_event_abs <- ifelse(df$response == 1, df$enroll + df$eff_time, Inf)
    }

    tox_rel <- df$tox_event_abs - df$enroll
    eff_rel <- df$eff_event_abs - df$enroll
    if (any(df$dlt == 1 & (tox_rel < 0 | tox_rel > td$A_T), na.rm = TRUE)) {
      errors <- c(errors, sprintf("toxicity event times for dlt = 1 must fall within [0, A_T = %.2f].", td$A_T))
    }
    if (any(df$response == 1 & (eff_rel < 0 | eff_rel > td$A_E), na.rm = TRUE)) {
      errors <- c(errors, sprintf("efficacy event times for response = 1 must fall within [0, A_E = %.2f].", td$A_E))
    }
  }

  if (length(errors) == 0) {
    dose_per_cohort <- stats::aggregate(dose ~ cohort, data = df, function(x) length(unique(x)))
    bad <- dose_per_cohort$cohort[dose_per_cohort$dose > 1]
    if (length(bad) > 0) {
      errors <- c(errors, sprintf(
        "Each cohort must have one assigned dose; multiple doses found in cohort(s): %s.",
        paste(bad, collapse = ", ")
      ))
    }
  }

  if (anyDuplicated(df$patient_id) > 0) {
    warnings <- c(warnings, "Duplicate patient_id values found; rows are still used as uploaded.")
  }
  if ("decision_time" %in% names(df) && any(is.na(df$decision_time))) {
    warnings <- c(warnings, "Some decision_time values are missing; actual next-cohort enrollment time will be used where needed.")
  }

  if (length(errors) > 0) {
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL, errors = errors, warnings = warnings))
  }

  df$cohort <- as.integer(df$cohort)
  df$dose <- as.integer(df$dose)
  df$dlt <- as.integer(df$dlt)
  df$response <- as.integer(df$response)
  df$tox_end <- df$enroll + td$A_T
  df$eff_end <- df$enroll + td$A_E
  df$tox_event <- ifelse(df$dlt == 1, df$tox_event_abs, Inf)
  df$eff_event <- ifelse(df$response == 1, df$eff_event_abs, Inf)
  df$tox_confirm <- pmin(df$tox_end, df$tox_event)
  df$eff_confirm <- pmin(df$eff_end, df$eff_event)

  out_cols <- c("patient_id", "cohort", "dose", "enroll", "pk", "dlt",
                "response", "tox_end", "eff_end", "tox_event", "eff_event",
                "tox_confirm", "eff_confirm")
  if ("decision_time" %in% names(df)) out_cols <- c(out_cols, "decision_time")
  df <- df[order(df$cohort, df$dose, df$enroll, df$patient_id), out_cols, drop = FALSE]
  rownames(df) <- NULL

  cd <- pkboin_patient_to_cohort(df)
  list(ok = TRUE, patients = df, cohort_data = cd, data = cd,
       errors = character(0), warnings = warnings)
}

tite_pkboin_uploaded_decision_time <- function(patient_df, cohorts, idx, current_cohort) {
  cur <- patient_df[patient_df$cohort == current_cohort, , drop = FALSE]
  if ("decision_time" %in% names(cur)) {
    dt <- unique(cur$decision_time[!is.na(cur$decision_time)])
    if (length(dt) > 0) return(min(dt))
  }
  if (idx < length(cohorts)) {
    nxt <- patient_df[patient_df$cohort == cohorts[idx + 1L], , drop = FALSE]
    return(min(nxt$enroll, na.rm = TRUE))
  }
  max(pmax(patient_df$tox_confirm, patient_df$eff_confirm), na.rm = TRUE)
}

tite_pkboin_replay_uploaded <- function(patient_df, design, boundaries,
                                        pk_design, tite_design, u, n_dose) {
  D <- n_dose
  if (is.null(pk_design$zeta1)) pk_design$zeta1 <- pkboin_zeta1(pk_design$r_P, pk_design$r_I_mult)

  eliminated <- rep(FALSE, D)
  pk_eliminated <- rep(FALSE, D)
  pk_terminate <- FALSE
  patient_df <- patient_df[order(patient_df$cohort, patient_df$dose,
                                 patient_df$enroll, patient_df$patient_id), , drop = FALSE]
  cohorts <- sort(unique(patient_df$cohort))
  log_rows <- vector("list", length(cohorts))

  for (i in seq_along(cohorts)) {
    cc <- cohorts[i]
    cur <- patient_df[patient_df$cohort == cc, , drop = FALSE]
    j <- unique(cur$dose)
    if (length(j) != 1) stop("Each uploaded cohort must have exactly one dose.")

    cum_pat <- patient_df[patient_df$cohort <= cc, , drop = FALSE]
    time_next <- tite_pkboin_uploaded_decision_time(patient_df, cohorts, i, cc)
    st <- tite_pkboin_patient_state(cum_pat, time_next, tite_design)
    qobs <- tite_pkboin_quasi_obs(st, D)
    pk_s <- tite_pkboin_pk_summary_from_patients(cum_pat, D)

    el <- boin_elimination(qobs[qobs$n > 0, c("dose", "n", "n1", "n2", "n3", "n4"), drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    pk_el <- pkboin_pk_elimination(pk_s$mean, pk_s$sd, pk_s$n,
                                   pk_design$r_P, pk_design$C_P)
    pk_eliminated <- pk_eliminated | pk_el$pk_elim
    pk_terminate <- pk_terminate || isTRUE(pk_el$terminate)

    nd <- pkboin_next_dose(j, qobs[, c("dose", "n", "n1", "n2", "n3", "n4")],
                           pk_s$mean, boundaries, u, D,
                           design = design, pk_design = pk_design)
    removed <- eliminated | pk_eliminated
    admissible <- nd$admissible[!removed[nd$admissible]]
    next_dose <- if (pk_terminate || length(admissible) == 0) {
      NA_integer_
    } else {
      admissible[which.max(nd$scores[as.character(admissible)])]
    }

    actual_next <- if (i < length(cohorts)) {
      as.integer(unique(patient_df$dose[patient_df$cohort == cohorts[i + 1L]]))
    } else {
      NA_integer_
    }
    protocol_deviation <- if (is.na(actual_next) || is.na(next_dose)) FALSE else actual_next != next_dose
    stop_reason <- if (pk_terminate) {
      "stop: top dose PK below target"
    } else if (length(admissible) == 0) {
      "stop: no admissible dose"
    } else {
      ""
    }
    decision <- if (is.na(next_dose)) {
      stop_reason
    } else if (next_dose > j) {
      "escalate"
    } else if (next_dose < j) {
      "de-escalate"
    } else {
      "stay"
    }

    complete_so_far <- tite_pkboin_complete_obs(cum_pat, D)
    cohort_complete <- tite_pkboin_complete_obs(cur, D)
    rds_str <- paste(sprintf("d%d=%.3f", nd$admissible, unname(nd$scores)), collapse = "  ")
    rds_next <- if (is.na(next_dose)) NA_real_ else unname(nd$scores[as.character(next_dose)])

    log_rows[[i]] <- data.frame(
      cohort = cc,
      actual_dose = j,
      decision_time = round(time_next, 2),
      cohort_n = cohort_complete$n[j],
      cohort_tox = cohort_complete$n2[j] + cohort_complete$n4[j],
      cohort_eff = cohort_complete$n1[j] + cohort_complete$n2[j],
      cum_n = complete_so_far$n[j],
      cum_tox = complete_so_far$n2[j] + complete_so_far$n4[j],
      cum_eff = complete_so_far$n1[j] + complete_so_far$n2[j],
      pending_t_current = qobs$pending_t[j],
      pending_e_current = qobs$pending_e[j],
      ESS_t_current = round(qobs$ESS_t[j], 3),
      ESS_e_current = round(qobs$ESS_e[j], 3),
      p_star = round(qobs$p_star[j], 4),
      q_star = round(qobs$q_star[j], 4),
      r_hat = ifelse(is.na(pk_s$mean[j]), NA_real_, round(pk_s$mean[j], 2)),
      pk_sd = pk_s$sd[j],
      pk_n = pk_s$n[j],
      pk_adequate = isTRUE(nd$pk_adequate),
      d_pk_min = nd$d_pk_min,
      d_star = nd$d_star,
      admissible = paste(nd$admissible, collapse = ","),
      admissible_after_elim = paste(admissible, collapse = ","),
      rds_by_dose = rds_str,
      rds_next = ifelse(is.na(rds_next), NA_real_, round(rds_next, 4)),
      tox_eff_eliminated = paste(which(eliminated), collapse = ","),
      pk_eliminated = paste(which(pk_eliminated), collapse = ","),
      recommended_next_dose = next_dose,
      actual_next_dose = actual_next,
      protocol_deviation = protocol_deviation,
      decision = decision,
      stop_reason = stop_reason
    )
  }

  complete_obs <- tite_pkboin_complete_obs(patient_df, D)
  pk_s <- tite_pkboin_pk_summary_from_patients(patient_df, D)
  final <- pkboin_select_obd(complete_obs, pk_s$mean, design,
                             list(r_P = pk_design$r_P), u, pk_eliminated)
  list(log = do.call(rbind, log_rows), obs = complete_obs,
       obs_pk = pk_s$mean, sigma_pk = pk_s$sd, pk_n = pk_s$n,
       eliminated = eliminated, pk_eliminated = pk_eliminated,
       final_obd = final)
}

tite_pkboin_upload_archive <- function(replay, patient_df, design, boundaries,
                                       pk_design, tite_design) {
  list(
    settings = data.frame(
      name = c("phi_T", "phi_E", "CT", "CE", "lambda_e", "lambda_d",
               "r_P", "r_I_mult", "zeta1", "C_P",
               "A_T", "A_E", "suspend_threshold"),
      value = c(design$phi_T, design$phi_E, design$CT, design$CE,
                boundaries$lambda_e, boundaries$lambda_d,
                pk_design$r_P, pk_design$r_I_mult, pk_design$zeta1, pk_design$C_P,
                tite_design$A_T, tite_design$A_E, tite_design$suspend_threshold)
    ),
    raw_patient_data = patient_df,
    replay_log = replay$log,
    final_obd = replay$final_obd$summary
  )
}
