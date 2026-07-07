# =====================================================================
# PKBOIN-12 upload-mode replay (pure functions)
# ---------------------------------------------------------------------
# uploaded patient-level data -> cohort-by-cohort deterministic replay
# -> final PKBOIN-12 OBD.
#
# Required input columns:
#   patient_id, cohort, dose, dlt, response, pk
#
# This file intentionally does NOT call simulation functions:
#   pkboin_operating_char(), pkboin_one_trial(),
#   pkboin_one_trial_traj(), pkboin_gen_cohort().
#
# It reuses the validated BOIN/PKBOIN decision primitives:
#   boin_elimination(), boin_rds(), pkboin_d_pk_min(),
#   pkboin_pk_elimination(), pkboin_next_dose(),
#   pkboin_select_obd().
# =====================================================================

pkboin_validate_upload <- function(raw, n_dose) {
  warnings <- character(0)
  required <- c("patient_id", "cohort", "dose", "dlt", "response", "pk")

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL, errors = "Uploaded file is empty or could not be read.",
                warnings = character(0)))
  }

  missing <- setdiff(required, colnames(raw))
  if (length(missing) > 0) {
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL,
                errors = sprintf(
                  "PKBOIN-12 upload requires patient-level columns: %s. Missing: %s.",
                  paste(required, collapse = ", "), paste(missing, collapse = ", ")
                ),
                warnings = character(0)))
  }

  df <- as.data.frame(raw[, required, drop = FALSE])
  errors <- character(0)

  df$cohort  <- suppressWarnings(as.numeric(df$cohort))
  df$dose    <- suppressWarnings(as.numeric(df$dose))
  df$dlt     <- suppressWarnings(as.numeric(df$dlt))
  df$response <- suppressWarnings(as.numeric(df$response))
  df$pk      <- suppressWarnings(as.numeric(df$pk))

  for (cc in c("cohort", "dose", "dlt", "response", "pk")) {
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

  if (length(errors) > 0) {
    return(list(ok = FALSE, patients = NULL, cohort_data = NULL,
                data = NULL, errors = errors, warnings = warnings))
  }

  df$cohort  <- as.integer(df$cohort)
  df$dose    <- as.integer(df$dose)
  df$dlt     <- as.integer(df$dlt)
  df$response <- as.integer(df$response)
  df <- df[order(df$cohort, df$dose, df$patient_id), , drop = FALSE]
  rownames(df) <- NULL

  cd <- pkboin_patient_to_cohort(df)
  list(ok = TRUE, patients = df, cohort_data = cd, data = cd,
       errors = character(0), warnings = warnings)
}

pkboin_patient_to_cohort <- function(patient_df) {
  df <- as.data.frame(patient_df)
  df$.n1 <- as.integer(df$response == 1 & df$dlt == 0)
  df$.n2 <- as.integer(df$response == 1 & df$dlt == 1)
  df$.n3 <- as.integer(df$response == 0 & df$dlt == 0)
  df$.n4 <- as.integer(df$response == 0 & df$dlt == 1)

  agg <- stats::aggregate(
    cbind(n = rep(1L, nrow(df)), n1 = df$.n1, n2 = df$.n2,
          n3 = df$.n3, n4 = df$.n4) ~ cohort + dose,
    data = df, FUN = sum
  )
  pk <- stats::aggregate(pk ~ cohort + dose, data = df, FUN = mean)
  names(pk)[names(pk) == "pk"] <- "cohort_pk_mean"
  out <- merge(agg, pk, by = c("cohort", "dose"), all.x = TRUE)
  out <- out[order(out$cohort), c("cohort", "dose", "n", "n1", "n2", "n3", "n4", "cohort_pk_mean")]
  rownames(out) <- NULL
  out
}

pkboin_cum_obs_from_patients <- function(patient_df, n_dose) {
  obs <- data.frame(dose = seq_len(n_dose), n = 0L,
                    n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  obs_pk <- rep(NA_real_, n_dose)
  sigma_pk <- rep(NA_real_, n_dose)
  pk_n <- integer(n_dose)

  if (!is.null(patient_df) && nrow(patient_df) > 0) {
    cd <- pkboin_patient_to_cohort(patient_df)
    for (i in seq_len(nrow(cd))) {
      j <- cd$dose[i]
      obs$n[j]  <- obs$n[j]  + cd$n[i]
      obs$n1[j] <- obs$n1[j] + cd$n1[i]
      obs$n2[j] <- obs$n2[j] + cd$n2[i]
      obs$n3[j] <- obs$n3[j] + cd$n3[i]
      obs$n4[j] <- obs$n4[j] + cd$n4[i]
    }
    for (d in seq_len(n_dose)) {
      x <- patient_df$pk[patient_df$dose == d]
      pk_n[d] <- length(x)
      if (pk_n[d] > 0) obs_pk[d] <- mean(x)
      if (pk_n[d] >= 2) sigma_pk[d] <- stats::sd(x)
    }
  }

  list(obs = obs, obs_pk = obs_pk, sigma_pk = sigma_pk, pk_n = pk_n)
}

pkboin_replay_uploaded <- function(patient_df, design, boundaries, pk_design, u, n_dose) {
  D <- n_dose
  if (is.null(pk_design$zeta1)) pk_design$zeta1 <- pkboin_zeta1(pk_design$r_P, pk_design$r_I_mult)

  obs <- data.frame(dose = seq_len(D), n = 0L,
                    n1 = 0L, n2 = 0L, n3 = 0L, n4 = 0L)
  obs_pk <- rep(NA_real_, D)
  sigma_pk <- rep(NA_real_, D)
  pk_n <- integer(D)
  eliminated <- rep(FALSE, D)
  pk_eliminated <- rep(FALSE, D)
  pk_terminate <- FALSE

  patient_df <- patient_df[order(patient_df$cohort, patient_df$dose, patient_df$patient_id), , drop = FALSE]
  cohorts <- sort(unique(patient_df$cohort))
  log_rows <- vector("list", length(cohorts))

  for (i in seq_along(cohorts)) {
    cc <- cohorts[i]
    cur <- patient_df[patient_df$cohort == cc, , drop = FALSE]
    j <- unique(cur$dose)
    if (length(j) != 1) stop("Each uploaded cohort must have exactly one dose.")

    cd <- pkboin_patient_to_cohort(cur)
    obs$n[j]  <- obs$n[j]  + cd$n
    obs$n1[j] <- obs$n1[j] + cd$n1
    obs$n2[j] <- obs$n2[j] + cd$n2
    obs$n3[j] <- obs$n3[j] + cd$n3
    obs$n4[j] <- obs$n4[j] + cd$n4

    cum_pat <- patient_df[patient_df$cohort <= cc, , drop = FALSE]
    for (d in seq_len(D)) {
      x <- cum_pat$pk[cum_pat$dose == d]
      pk_n[d] <- length(x)
      obs_pk[d] <- if (pk_n[d] > 0) mean(x) else NA_real_
      sigma_pk[d] <- if (pk_n[d] >= 2) stats::sd(x) else NA_real_
    }

    el <- boin_elimination(obs[obs$n > 0, , drop = FALSE],
                           design$phi_T, design$phi_E, design$CT, design$CE)
    eliminated[el$dose] <- el$eliminated

    pk_el <- pkboin_pk_elimination(obs_pk, sigma_pk, pk_n,
                                   pk_design$r_P, pk_design$C_P)
    pk_eliminated <- pk_eliminated | pk_el$pk_elim
    pk_terminate <- pk_terminate || isTRUE(pk_el$terminate)

    nd <- pkboin_next_dose(j, obs, obs_pk, boundaries, u, D,
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

    rds_str <- paste(sprintf("d%d=%.3f", nd$admissible, unname(nd$scores)), collapse = "  ")
    rds_next <- if (is.na(next_dose)) NA_real_ else unname(nd$scores[as.character(next_dose)])
    cur_tox_rate <- if (obs$n[j] > 0) (obs$n2[j] + obs$n4[j]) / obs$n[j] else 0
    decision <- if (is.na(next_dose)) {
      stop_reason
    } else if (next_dose > j) {
      "escalate"
    } else if (next_dose < j) {
      "de-escalate"
    } else {
      "stay"
    }

    log_rows[[i]] <- data.frame(
      cohort = cc,
      actual_dose = j,
      cohort_n = cd$n,
      cohort_tox = cd$n2 + cd$n4,
      cohort_eff = cd$n1 + cd$n2,
      cum_n = obs$n[j],
      cum_tox = obs$n2[j] + obs$n4[j],
      cum_eff = obs$n1[j] + obs$n2[j],
      p_hat = cur_tox_rate,
      r_hat = obs_pk[j],
      pk_sd = sigma_pk[j],
      pk_n = pk_n[j],
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

  final <- pkboin_select_obd(obs, obs_pk, design,
                             list(r_P = pk_design$r_P), u, pk_eliminated)
  list(log = do.call(rbind, log_rows), obs = obs, obs_pk = obs_pk,
       sigma_pk = sigma_pk, pk_n = pk_n, eliminated = eliminated,
       pk_eliminated = pk_eliminated, final_obd = final)
}

pkboin_upload_archive <- function(replay, patient_df, design, boundaries, pk_design) {
  list(
    settings = data.frame(
      name = c("phi_T", "phi_E", "CT", "CE", "lambda_e", "lambda_d", "r_P", "r_I_mult", "zeta1", "C_P"),
      value = c(design$phi_T, design$phi_E, design$CT, design$CE,
                boundaries$lambda_e, boundaries$lambda_d,
                pk_design$r_P, pk_design$r_I_mult, pk_design$zeta1, pk_design$C_P)
    ),
    raw_patient_data = patient_df,
    replay_log = replay$log,
    final_obd = replay$final_obd$summary
  )
}
