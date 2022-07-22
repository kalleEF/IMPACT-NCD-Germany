
#### Scenarios for German SSB tax modelling ------------------------------------

### Scenario 1 20% SSB tax without substitution to fruit juice ----------------

scenario_fn <- function(sp) {
  
  tax <- 20
  pass_through <- 0.82
  oPE_SSB <- -1.2 # Own-price elasticity of SSBs #TODO Make probabilistic
  
  #tbl <- read_fst(.inputsother_parametersoPE_SSB_rr.fst)
  #oPE_SSB <- as.numeric(tbl[mc == sp$mc_aggr, oPE_SSB])
  
  policy_lag <- 0 # Lag until policy affects consumption in years
  bmi_lag <- 5 # Lag until full BMI reduction is achieved #TODO Make probabilistic
  bmi_steps <- 1/bmi_lag
  
  tbl <- as.data.table(read_fst("./inputs/other_parameters/sugar_rr.fst"))
  sugar_rr_low <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_low"])
  sugar_rr_high <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_high"])
  
  sp$pop[, ssb_delta_xps := ssb_curr_xps - (ssb_curr_xps * (1 + oPE_SSB * ((tax/100) * pass_through)))]
  sp$pop[, sugar_delta := ssb_delta_xps * sugar_per_ssb]
  sp$pop[, bmi_delta := fifelse(bmi_curr_xps < 25,
                                sugar_delta * sugar_rr_low,
                                sugar_delta * sugar_rr_high)]
  
  sp$pop[, bmi_mod := fifelse(year >= (13 + policy_lag) & year <= (13 + policy_lag + bmi_lag),
                              (year - (13 + policy_lag)) * bmi_steps,
                              1)]
  
  sp$pop[, bmi_check := bmi_curr_xps]
  sp$pop[year >= (13 + policy_lag), bmi_check := bmi_curr_xps - (bmi_delta * bmi_mod)]
  
  print("BMI CHECK START")
  print(sp$pop[, all(bmi_check <= bmi_curr_xps)])
  print("BMI CHECK END")
  
  sp$pop[year >= (13 + policy_lag), bmi_curr_xps := bmi_check]
  
  sp$pop[, c("ssb_delta_xps", "sugar_delta", "bmi_delta", "bmi_mod", "bmi_check") := NULL]
  
}