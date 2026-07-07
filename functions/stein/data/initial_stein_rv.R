# =====================================================================
# STEIN reactiveValues subtree
# ---------------------------------------------------------------------
# Fully isolated from the CUS/PKBOIN `all_rv` tree. Instantiated once in
# app.R server as `stein_rv <- initial_stein_rv()`. Nothing here touches
# or reads the existing all_rv structure, so the CUS and PKBOIN reactive
# graphs are guaranteed untouched.
#
# Auto-sourced by app.R via list.files("functions", recursive = TRUE).
# =====================================================================

initial_stein_rv <- function() {

  # ---- design parameters (Design tab) --------------------------------
  # defaults follow the STEIN paper conventions:
  #   phi1 = 0.75 * phi0 , phi2 = 1.25 * phi0 (toxicity anchors)
  #   psi1 = 0.30 , psi2 = 0.80 (efficacy uninteresting / desirable)
  #   w1 = 0.33 , w2 = 1.09 (utility toxicity-penalty weights)
  #   CT = 0.95 (toxicity elimination) , CE = 0.98 (efficacy elimination)
  design_setting <- reactiveValues(
    phi0 = 0.35,   # target toxicity probability
    phi1 = NULL,   # lower toxicity anchor (auto = 0.75 * phi0 unless overridden)
    phi2 = NULL,   # upper toxicity anchor (auto = 1.25 * phi0 unless overridden)
    phi1_mult = 0.75,
    phi2_mult = 1.25,
    psi1 = 0.30,   # clinically uninteresting response rate
    psi2 = 0.80,   # clinically desirable response rate
    w1   = 0.33,   # utility weight on toxicity
    w2   = 1.09,   # extra utility penalty when p > phi0
    CT   = 0.95,   # toxicity elimination cutoff
    CE   = 0.98    # efficacy elimination cutoff
  )

  # ---- trial structure -----------------------------------------------
  trial_setting <- reactiveValues(
    n_dose      = 5,   # number of dose levels D
    start_dose  = 1,   # starting dose level
    cohort_size = 3,   # patients per cohort
    n_max       = 30   # maximum sample size
  )

  # ---- simulate vs upload flag ---------------------------------------
  overall_setting <- reactiveValues(
    simu_or_not = 1    # 1 = simulate, 2 = upload
  )

  # ---- simulate-only: scenario true rates (Scenario tab) -------------
  # p_true / q_true are per-dose truth used to generate virtual patients.
  scenario_setting <- reactiveValues(
    shape    = "unimodal",                      # increasing/plateau/unimodal/constant
    p_true   = c(0.05, 0.15, 0.30, 0.45, 0.55), # per-dose true toxicity
    q_true   = c(0.20, 0.40, 0.55, 0.45, 0.35), # per-dose true efficacy
    n_rep    = 2000                             # simulation replications
  )

  # ---- conduct / upload: observed trial data -------------------------
  # summary-level per-dose counts; upload mode fills this from a file,
  # conduct mode fills it cohort by cohort.
  trial_data <- reactiveValues(
    obs = data.frame(
      dose  = integer(0),
      n     = integer(0),
      n_dlt = integer(0),
      n_eff = integer(0)
    ),
    cohort_data = NULL, # validated upload (upload mode): cohort, dose, n, n_dlt, n_eff
                        # shared source for both Trial Conduct and OBD Selection tabs
    upload_raw = NULL   # raw uploaded data.frame (pre-validation)
  )

  # ---- triggers (isolated namespace) ---------------------------------
  triggers <- reactiveValues(
    recompute  = NULL,
    sim_active = FALSE   # becomes TRUE after first "Run simulation" click (OC tab);
                         # gates auto-update in both OC and Data tabs thereafter
  )

  list(
    design_setting   = design_setting,
    trial_setting    = trial_setting,
    overall_setting  = overall_setting,
    scenario_setting = scenario_setting,
    trial_data       = trial_data,
    triggers         = triggers
  )
}
