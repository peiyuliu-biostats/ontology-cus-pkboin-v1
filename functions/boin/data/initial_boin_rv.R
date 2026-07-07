initial_boin_rv <- function() {
  
  overall_setting <- reactiveValues(
    method = "BOIN12",
    simu_or_not = 1
  )
  
  design_setting <- reactiveValues(
    phi_T = 0.35,
    phi_E = 0.25,
    phi1_mult = 0.6,
    phi2_mult = 1.4,
    phi1 = NULL,
    phi2 = NULL,
    u1 = 100,
    u2 = 60,
    u3 = 40,
    u4 = 0,
    CT = 0.95,
    CE = 0.90,
    obj_mode = "max_utility",
    w = 1
  )
  
  .r_P_default <- 6000
  .r_I_mult_default <- 0.6
  pk_setting <- reactiveValues(
    r_P = .r_P_default,
    r_I_mult = .r_I_mult_default,
    r_I = .r_I_mult_default * .r_P_default,
    zeta1 = (1 + .r_I_mult_default) / 2 * .r_P_default,
    C_P = 0.95,
    CV = 0.25,
    g_P = 1.0
  )
  
  tite_setting <- reactiveValues(
    A_T = 30,
    A_E = 60,
    accrual_rate = 10,
    suspend_threshold = 0.5,
    use_susp = TRUE,
    accrual_random = FALSE,
    current_time = NULL
  )
  
  trial_setting <- reactiveValues(
    n_dose = 5,
    start_dose = 1,
    cohort_size = 3,
    n_max = 30
  )
  
  scenario_setting <- reactiveValues(
    shape = "unimodal",
    p_true = c(0.05, 0.15, 0.30, 0.45, 0.55),
    q_true = c(0.20, 0.40, 0.55, 0.45, 0.35),
    r_true = c(2000, 4000, 6000, 7500, 8500),
    n_rep = 2000
  )
  
  trial_data <- reactiveValues(
    obs = data.frame(
      dose = integer(0), n = integer(0),
      n1 = integer(0), n2 = integer(0), n3 = integer(0), n4 = integer(0)
    ),
    cohort_data = NULL,
    patient_data = NULL,
    upload_method = NULL,
    upload_raw = NULL
  )
  
  triggers <- reactiveValues(
    recompute = NULL,
    sim_active = FALSE,
    sim_running = FALSE,
    sim_run_id = 0L,
    sim_signature = NULL,
    sim_method = NULL
  )

  results <- reactiveValues(
    sim = NULL
  )
  
  list(
    overall_setting = overall_setting,
    design_setting = design_setting,
    pk_setting = pk_setting,
    tite_setting = tite_setting,
    trial_setting = trial_setting,
    scenario_setting = scenario_setting,
    trial_data = trial_data,
    triggers = triggers,
    results = results
  )
}
