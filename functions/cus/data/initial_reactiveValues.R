#### Overall Setting #### 
initial_overall_setting <- function()
{
  overall_setting <- reactiveValues(
    simu_or_not = 1,               # Simulate or upload the ER dataset?
    eff_PK_model = 1,              # Select the regression model for PK-Efficacy
    safe_PK_model = 1,             # Select the regression model for PK-Safety
    utility_type = 1,              # S-shape or stepwise
    individual_Sshape_utility = 2,     # 1: Yes; 2: No
    individual_stepwise_utility = 2,   # 1: Yes; 2: No
    cus_agg_type = 1,                  # CUS aggregation: 1 = multiplicative (default), 2 = linear
    # uploaded-data endpoint type: NA = not uploaded (no restriction), TRUE = binary, FALSE = continuous
    eff_data_binary = NA,
    safe_data_binary = NA,
    # SIMULATE mixed endpoints (B scheme): per-endpoint type + one shared regression per type.
    # type_vec[i] in {"bin","cont"}; bin endpoints share *_bin_model (1=Logistic,2=Emax);
    # cont endpoints share *_cont_model (3=Linear,4=Log-linear,5=Exp). defaults reproduce
    # the current single-Logistic behavior. upload mode still uses the scalar *_PK_model above.
    eff_type_vec  = rep("bin", 10),
    safe_type_vec = rep("bin", 10),
    eff_bin_model  = 1, eff_cont_model  = 3,
    safe_bin_model = 1, safe_cont_model = 3
  )
  
  return(overall_setting)
}

#### Trigger #### 
initial_triggers <- function()
{
  triggers <- reactiveValues(
    update_ER_dataset = Sys.time(),
    update_endpoint_Emax_trigger = Sys.time(),
    update_utility_Sshape_indiv_trigger = Sys.time(),
    update_utility_stepwise_indiv_trigger = Sys.time()
  )
  return(triggers)
}

#### All Data Part #### 
initial_ER_data_list <- function()
{
  ER_data_list <- 
    reactiveValues(ER_rawdt = data.frame(), 
                   eff_logistic_reg = list(),
                   safe_logistic_reg = list(),
                   eff_Emax_reg = list(), 
                   safe_Emax_reg = list()
    )
  
  return(ER_data_list)
}

#### PK setting ####
initial_PK_setting <- function()
{
  PK_setting <- reactiveValues(
    PK_min = 1,
    PK_max = 2
  )
  
  return(PK_setting)
}


#### Number of Endpoint setting ####
initial_endpoint_num_setting <- function()
{
  endpoint_num_setting <- reactiveValues(
    eff_num = 1,
    safe_num = 2
  )
  return(endpoint_num_setting)
}

#### Efficacy Endpoint parameters ####
initial_eff_endpoint_setting <- function()
{
  eff_endpoint_setting <- reactiveValues(
    eff_weight = rep(1, 10),
    # sigmoid
    eff_slope = rep(0, 10),
    eff_intercept = rep(0, 10),
    # Emax
    eff_baseline = rep(0, 10),
    eff_Emax = rep(1, 10),
    eff_EC50 = rep(1, 10),
    eff_hill = rep(1, 10),
    # continuous (upload): fitted intercept/slope for linear / log-linear / exponential
    eff_lin_a = rep(0, 10), eff_lin_b = rep(0, 10),
    eff_log_a = rep(0, 10), eff_log_b = rep(0, 10),
    eff_exp_a = rep(0, 10), eff_exp_b = rep(0, 10),
    # continuous (upload): optional user response bounds; NA = empty = no clipping.
    # Yhat is clipped to [lower, upper] before the ECDF map (initial_PK_data + bootstrap).
    eff_resp_lb = rep(NA_real_, 10), eff_resp_ub = rep(NA_real_, 10)
  )
  return(eff_endpoint_setting)
}

#### Safety Endpoint parameters ####
initial_safe_endpoint_setting <- function()
{
  safe_endpoint_setting <- reactiveValues(
    safe_weight = rep(1, 10),
    # sigmoid
    safe_slope = rep(0, 10),
    safe_intercept = rep(0, 10),
    # Emax 
    safe_baseline = rep(0, 10),
    safe_Emax = rep(1, 10),
    safe_EC50 = rep(1, 10),
    safe_hill = rep(1, 10),
    # continuous (upload): fitted intercept/slope for linear / log-linear / exponential
    safe_lin_a = rep(0, 10), safe_lin_b = rep(0, 10),
    safe_log_a = rep(0, 10), safe_log_b = rep(0, 10),
    safe_exp_a = rep(0, 10), safe_exp_b = rep(0, 10),
    # continuous (upload): optional user response bounds; NA = empty = no clipping.
    safe_resp_lb = rep(NA_real_, 10), safe_resp_ub = rep(NA_real_, 10)
  )
  return(safe_endpoint_setting)
}

#### Utility Function #####
initial_utility_Sshape_setting <- function()
{
  utility_Sshape_setting <- reactiveValues(
    eff_beta = 1, eff_shape = 0.5, 
    safe_beta = 1, safe_shape = 0.5
  )
  
  return(utility_Sshape_setting)
}

initial_inidividual_utility_Sshape_setting <- function()
{
  inidividual_utility_Sshape_setting <- reactiveValues(
    eff_beta = rep(1, 10),
    eff_shape = rep(0.5, 10),  
    safe_beta = rep(1, 10), 
    safe_shape = rep(0.5, 10)
  )
  
  return(inidividual_utility_Sshape_setting)
}

initial_utility_stepwise_setting <- function()
{
  utility_stepwise_setting <- reactiveValues(
    upload_or_not = 1,
    eff_knot_num = 4, 
    safe_knot_num = 4,
    eff_measure = c(0.2, 0.4, 0.6, 0.8), 
    eff_score = c(0.2, 0.4, 0.6, 0.8),
    safe_measure = c(0.2, 0.4, 0.6, 0.8), 
    safe_score = c(0.8, 0.6, 0.4, 0.2)
  )
  
  return(utility_stepwise_setting)
}

initial_individual_utility_stepwise_setting <- function()
{
  individual_utility_stepwise_setting <- reactiveValues(
    upload_or_not = 1,
    eff_knot_num = rep(4, 10),
    safe_knot_num = rep(4, 10),
    eff_measure = replicate(10, c(0.2, 0.4, 0.6, 0.8), simplify = FALSE),
    eff_score = replicate(10, c(0.2, 0.4, 0.6, 0.8), simplify = FALSE),
    safe_measure = replicate(10, c(0.2, 0.4, 0.6, 0.8), simplify = FALSE),
    safe_score = replicate(10, c(0.8, 0.6, 0.4, 0.2), simplify = FALSE)
  )
  
  return(individual_utility_stepwise_setting)
}