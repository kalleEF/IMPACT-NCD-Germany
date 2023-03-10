
#### Scenarios for German SSB tax modelling ------------------------------------

scenario_0_fn <- function(sp) {
  
  sp$pop[, c("ssb_delta_xps", "juice_delta_xps", "sugar_delta", "bmi_delta") := 0] 
  
}

### Scenario 1 - 20% ad valorem tax on all drinks with added caloric sweeteners ----

scenario_1_fn <- function(sp) {
  
  # Set scenario variables #
  tax <- 20
  
  tbl <- read_fst("./inputs/other_parameters/tax_pass_through.fst", as.data.table = TRUE)
  pass_through <- as.numeric(tbl[mc == sp$mc_aggr, "tax_pth"])  # Based on meta-analysis Adreyeva et al. JAMA 2022

  tbl <- read_fst("./inputs/other_parameters/oPE_ssb.fst", as.data.table = TRUE)
  oPE_ssb <- as.numeric(tbl[mc == sp$mc_aggr, "oPE_ssb"]) # Own-price elasticity of SSBs
  
  tbl <- read_fst("./inputs/other_parameters/cPE_ssb_juice.fst", as.data.table = TRUE)
  cPE_ssb_juice <- as.numeric(tbl[mc == sp$mc_aggr, "cPE_ssb_juice"]) # Cross-price elasticity of SSBs and fruit juice
  
  policy_lag <- 0 # Lag until policy affects consumption in years
  
  tbl <- read_fst("./inputs/other_parameters/bmi_lag.fst", as.data.table = TRUE)
  bmi_lag <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_lag"]) # Lag until full BMI reduction is achieved
  bmi_steps <- 1/bmi_lag
  
  tbl <- read_fst("./inputs/other_parameters/sugar_rr.fst", as.data.table = TRUE)
  sugar_rr_low <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_low"]) # Change in BMI due to sugar by BMI ><25
  sugar_rr_high <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_high"])
  
  # Change in SSB consumption after tax #
  sp$pop[, ssb_delta_xps := ssb_curr_xps - (ssb_curr_xps * (1 + oPE_ssb * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), ssb_curr_xps := ssb_curr_xps - (ssb_delta_xps)]
  
  # Change in fruit juice consumption after tax (substitution) #
  sp$pop[, juice_delta_xps := juice_curr_xps - (juice_curr_xps * (1 + cPE_ssb_juice * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), juice_curr_xps := juice_curr_xps - (juice_delta_xps)]
  
  # Change in consumption of sugar from SSBs after tax #
  sp$pop[, sugar_delta := (ssb_delta_xps * sugar_per_ssb) + (juice_delta_xps * sugar_per_juice)]
  
  # Change in BMI after tax #
  sp$pop[, bmi_delta := fifelse(bmi_curr_xps < 25,
                                sugar_delta * sugar_rr_low,
                                sugar_delta * sugar_rr_high)]
  
  # Lagged effect of BMI #
  sp$pop[, bmi_mod := 0]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), bmi_mod := fifelse(year > ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag + bmi_lag),
                                      (year - ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag)) * bmi_steps,
                                       1)]
  
  # New BMI under taxation scenario #
  sp$pop[year > ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag), bmi_curr_xps := bmi_curr_xps - (bmi_delta * bmi_mod)]
  
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("bmi_mod") := NULL]
  
}

### Scenario 2 - 20% ad valorem tax on all drinks with added caloric sweeteners and fruit juice ----

scenario_2_fn <- function(sp) {
  
  # Set scenario variables #
  tax <- 20
  
  tbl <- read_fst("./inputs/other_parameters/tax_pass_through.fst", as.data.table = TRUE)
  pass_through <- as.numeric(tbl[mc == sp$mc_aggr, "tax_pth"])  # Based on meta-analysis Adreyeva et al. JAMA 2022
  
  tbl <- read_fst("./inputs/other_parameters/oPE_ssb.fst", as.data.table = TRUE)
  oPE_ssb <- as.numeric(tbl[mc == sp$mc_aggr, "oPE_ssb"]) # Own-price elasticity of SSBs
  
  tbl <- read_fst("./inputs/other_parameters/oPE_juice.fst", as.data.table = TRUE)
  oPE_juice <- as.numeric(tbl[mc == sp$mc_aggr, "oPE_juice"]) # Own-price elasticity of fruit juice
  
  policy_lag <- 0 # Lag until policy affects consumption in years
  
  tbl <- read_fst("./inputs/other_parameters/bmi_lag.fst", as.data.table = TRUE)
  bmi_lag <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_lag"]) # Lag until full BMI reduction is achieved
  bmi_steps <- 1/bmi_lag
  
  tbl <- read_fst("./inputs/other_parameters/sugar_rr.fst", as.data.table = TRUE)
  sugar_rr_low <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_low"]) # Change in BMI due to sugar by BMI ><25
  sugar_rr_high <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_high"])
  
  # Change in SSB consumption after tax #
  sp$pop[, ssb_delta_xps := ssb_curr_xps - (ssb_curr_xps * (1 + oPE_ssb * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), ssb_curr_xps := ssb_curr_xps - (ssb_delta_xps)]
  
  # Change in fruit juice consumption after tax #
  sp$pop[, juice_delta_xps := juice_curr_xps - (juice_curr_xps * (1 + oPE_juice * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), juice_curr_xps := juice_curr_xps - (juice_delta_xps)]
  
  # Change in consumption of sugar from SSBs after tax #
  sp$pop[, sugar_delta := (ssb_delta_xps * sugar_per_ssb) + (juice_delta_xps * sugar_per_juice)]
  
  # Change in BMI after tax #
  sp$pop[, bmi_delta := fifelse(bmi_curr_xps < 25,
                                sugar_delta * sugar_rr_low,
                                sugar_delta * sugar_rr_high)]
  
  # Lagged effect of BMI #
  sp$pop[, bmi_mod := 0]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), bmi_mod := fifelse(year > ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag + bmi_lag),
                                       (year - ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag)) * bmi_steps,
                                       1)]
  
  # New BMI under taxation scenario #
  sp$pop[year > ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag), bmi_curr_xps := bmi_curr_xps - (bmi_delta * bmi_mod)]
  
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("bmi_mod") := NULL]
  
}


### Scenario 3 - 20% tiered tax with hypothetical thresholds leading to reformulation (30% less sugar) ----

scenario_3_fn <- function(sp) {
  
  # Set scenario variables #
  ref <- 1 - 0.3

  tbl <- read_fst("./inputs/other_parameters/bmi_lag.fst", as.data.table = TRUE)
  bmi_lag <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_lag"]) # Lag until full BMI reduction is achieved
  bmi_steps <- 1/bmi_lag
  
  tbl <- read_fst("./inputs/other_parameters/sugar_rr.fst", as.data.table = TRUE)
  sugar_rr_low <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_low"]) # Change in BMI due to sugar by BMI ><25
  sugar_rr_high <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_high"])
  
  # Reformulation effect over 3 years #
  ref_lag <- 3
  ref_steps <- 1/ref_lag
  
  sp$pop[, ref_mod := 1]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), ref_mod := fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + ref_lag),
                                       1 - (year - (IMPACTncd$design$sim_prm$init_year_intv - 2000)) * (1 - ref) * ref_steps,
                                       ref)]
  
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), sugar_per_ssb_new := sugar_per_ssb * ref_mod]

  # Change in consumption of sugar from SSBs after tax #
  sp$pop[, sugar_delta := ssb_sugar - (ssb_curr_xps * sugar_per_ssb_new)]
  
  # Recalculate SSB consumption to account for direct effect of reduced sugar SSBs #
  sp$pop[!is.na(sugar_delta), ssb_curr_xps := ssb_curr_xps - sugar_delta / sugar_per_ssb]
  
  
  # Change in BMI after tax #
  sp$pop[, bmi_delta := fifelse(bmi_curr_xps < 25,
                                sugar_delta * sugar_rr_low,
                                sugar_delta * sugar_rr_high)]
  
  # Lagged effect of BMI #
  sp$pop[, bmi_mod := 0]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), bmi_mod := fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + bmi_lag),
                                       (year - (IMPACTncd$design$sim_prm$init_year_intv - 2000)) * bmi_steps,
                                       1)]
  
  # New BMI under taxation scenario #
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), bmi_curr_xps := bmi_curr_xps - (bmi_delta * bmi_mod)]
  
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("ref_mod", "bmi_mod", "sugar_per_ssb_new") := NULL]
  sp$pop[, c("ssb_delta_xps", "juice_delta_xps") := 0] 
  
}


### Scenario 4 - 20% tiered tax with hypothetical thresholds leading to reformulation (30% less sugar) ----
#               (change in consumption and reformulation)

scenario_4_fn <- function(sp) {
  
  # Set scenario variables #
  ref <- 1 - 0.3
  tax <- 20
  
  tbl <- read_fst("./inputs/other_parameters/tax_pass_through.fst", as.data.table = TRUE)
  pass_through <- as.numeric(tbl[mc == sp$mc_aggr, "tax_pth"])  # Based on meta-analysis Adreyeva et al. JAMA 2022
  
  tbl <- read_fst("./inputs/other_parameters/oPE_ssb.fst", as.data.table = TRUE)
  oPE_ssb <- as.numeric(tbl[mc == sp$mc_aggr, "oPE_ssb"]) # Own-price elasticity of SSBs
  
  tbl <- read_fst("./inputs/other_parameters/cPE_ssb_juice.fst", as.data.table = TRUE)
  cPE_ssb_juice <- as.numeric(tbl[mc == sp$mc_aggr, "cPE_ssb_juice"]) # Cross-price elasticity of SSBs and fruit juice
  
  policy_lag <- 0 # Lag until policy affects consumption in years
  
  tbl <- read_fst("./inputs/other_parameters/bmi_lag.fst", as.data.table = TRUE)
  bmi_lag <- as.numeric(tbl[mc == sp$mc_aggr, "bmi_lag"]) # Lag until full BMI reduction is achieved
  bmi_steps <- 1/bmi_lag
  
  tbl <- read_fst("./inputs/other_parameters/sugar_rr.fst", as.data.table = TRUE)
  sugar_rr_low <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_low"]) # Change in BMI due to sugar by BMI ><25
  sugar_rr_high <- as.numeric(tbl[mc == sp$mc_aggr, "sugar_rr_high"])
  
  # Reformulation effect over 3 years #
  ref_lag <- 3
  ref_steps <- 1/ref_lag
  
  sp$pop[, ref_mod := 1]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), ref_mod := fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + ref_lag),
                                       1 - (year - (IMPACTncd$design$sim_prm$init_year_intv - 2000)) * (1 - ref) * ref_steps,
                                       ref)]
  
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), sugar_per_ssb_new := sugar_per_ssb * ref_mod]
  
  # Change in SSB consumption after tax #
  sp$pop[, ssb_delta_xps := ssb_curr_xps - (ssb_curr_xps * (1 + oPE_ssb * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), ssb_curr_xps := ssb_curr_xps - (ssb_delta_xps)]
  
  # Change in fruit juice consumption after tax #
  sp$pop[, juice_delta_xps := juice_curr_xps - (juice_curr_xps * (1 + cPE_ssb_juice * ((tax/100) * pass_through)))]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), juice_curr_xps := juice_curr_xps - (juice_delta_xps)]
  
  # Change in consumption of sugar from SSBs and juice after tax #
  sp$pop[, ssb_sugar_delta := (ssb_sugar - ssb_curr_xps * sugar_per_ssb_new)]

  # Change in consumption of sugar from SSBs after tax #
  sp$pop[, juice_sugar_delta := (juice_delta_xps * sugar_per_juice)]
  
  # Calculate additional delta of SSB consumption to account for direct effect of reduced sugar SSBs #
  sp$pop[, ssb_xps_reform_delta := ssb_sugar_delta / sugar_per_ssb]
  
  # Implement reformulation effect via SSB consumption #
  sp$pop[!is.na(ssb_xps_reform_delta), ssb_curr_xps := ssb_curr_xps - (ssb_xps_reform_delta)]
  
  # Change in BMI after tax #
  sp$pop[, bmi_delta := fifelse(bmi_curr_xps < 25,
                                (ssb_sugar_delta + juice_sugar_delta) * sugar_rr_low,
                                (ssb_sugar_delta + juice_sugar_delta) * sugar_rr_high)]
  
  sp$pop[, sugar_delta := (ssb_sugar_delta + juice_sugar_delta)][, `:=`(ssb_sugar_delta = NULL,
                                                                        juice_sugar_delta = NULL)]
  
  # Lagged effect of BMI #
  sp$pop[, bmi_mod := 0]
  sp$pop[year > (IMPACTncd$design$sim_prm$init_year_intv - 2000), bmi_mod := fifelse(year > (IMPACTncd$design$sim_prm$init_year_intv - 2000) & year <= ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + bmi_lag),
                                       (year - (IMPACTncd$design$sim_prm$init_year_intv - 2000)) * bmi_steps,
                                       1)]
  
  # New BMI under taxation scenario #
  sp$pop[year > ((IMPACTncd$design$sim_prm$init_year_intv - 2000) + policy_lag), bmi_curr_xps := bmi_curr_xps - (bmi_delta * bmi_mod)]
  
  # Delete unnecessary variables from synthpop #
  sp$pop[, c("bmi_mod", "ref_mod", "sugar_per_ssb_new", "ssb_xps_reform_delta") := NULL]
  
}


