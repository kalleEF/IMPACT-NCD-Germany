
## Health Economic Analyses ##

library(data.table)
library(CKutils)
library(doParallel)
library(fst)


if (Sys.info()["sysname"] == "Windows") {
  cl <-
    makeCluster(detectCores()) # used for clustering. Windows compatible
  registerDoParallel(cl)
} else {
  registerDoParallel(detectCores()) # used for forking. Only Linux/OSX compatible
}

## Computation of QALYs and costs per MC iteration ##

iterations <- list.files("./outputs/lifecourse/")

out <- foreach(i = iterations,
               .packages = c("data.table",
                             "CKutils",
                             "fst")) %dopar% {
  
  # Load lifecourse file #
  
  dt <- fread(paste0("./outputs/lifecourse/", i))
  
  # Set MC iteration #
  
  mc_ <- as.integer(gsub("_lifecourse.csv.gz", "", i))
  
  # Load health utility data #
    
  util_indx <- read_fst("./inputs/other_parameters/health_utility_indx.fst", as.data.table = TRUE)
  
  ro <- util_indx[
    mc %in% mc_, 
    .("from" = min(from), "to" = max(to))
  ]
  
  util <- read_fst(
    "./inputs/other_parameters/health_utility.fst",
    as.data.table = TRUE,
    from = ro$from,
    to = ro$to
  )
  
  # Load healthcare cost data #
  
  cost_indx <- read_fst("./inputs/other_parameters/healthcare_costs_indx.fst", as.data.table = TRUE)
  
  ro <- cost_indx[
    mc %in% mc_, 
    .("from" = min(from), "to" = max(to))
  ]
  
  cost <- read_fst(
    "./inputs/other_parameters/healthcare_costs.fst",
    as.data.table = TRUE,
    from = ro$from,
    to = ro$to
  )
  
  # Setup lifecourse #
  
  dt[, `:=`(t2dm_stat = fifelse(t2dm_prvl > 1, 1, 0),
            chd_stat = fifelse(chd_prvl == 1, 1,
                               fifelse(chd_prvl > 1, 2,
                                       fifelse(chd_prvl > 0 & all_cause_mrtl == 2, 3, 0))), # Mortality from CHD
            stroke_stat = fifelse(stroke_prvl == 1, 1,
                               fifelse(stroke_prvl > 1, 2,
                                       fifelse(stroke_prvl > 0 & all_cause_mrtl == 3, 3, 0))))] # Mortality from stroke
  
  to_agegrp(dt, grp_width = 10, min_age = 50, max_age = 80, agegrp_colname = "age_cost")
  dt[is.na(age_cost), age_cost := "<50"]
  
  absorb_dt(dt, util)
  absorb_dt(dt, cost)
  
  # Calculate health utility #
  
  dt[, health_util := util_incpt + age * util_age + util_sex + bmi_curr_xps * util_bmi + util_disease]
  
  # Setup results object #
  
  cea <- CJ(scenario = unique(dt$scenario),
            pid = unique(dt$pid))
  
  # Calculate QALYs #
  
  qalys_out <- dt[, lapply(.SD, function(x){
              
     if(length(x) > 1) {
       
       q <- integrate(approxfun(c(1:length(x)), x),
                      lower = range(c(1:length(x)))[1],
                      upper = range(c(1:length(x)))[2])$value
       
     } else {
       
       q <- x # For individuals with only one year per scenario
       
     }
     
     return(q)
    
   }), .SDcols = "health_util", by = .(pid, sex, scenario)]
  
  setnames(qalys_out, "health_util", "qalys_out")
  
  absorb_dt(cea, qalys_out)
  
  qalys_scaled <- dt[, lapply(.SD, function(x){
    
    if(length(x) > 1) {
      
      q <- integrate(approxfun(c(1:length(x)), x), 
                     lower = range(c(1:length(x)))[1],
                     upper = range(c(1:length(x)))[2])$value
      
      q <- q * sum(wt)
      
    } else {
      
      q <- x * wt # For individuals with only one year per scenario
      
    }
    
    return(q) 
    
  }), .SDcols = "health_util", by = .(pid, sex, scenario)]
  
  setnames(qalys_scaled, "health_util", "qalys_scl")
  
  absorb_dt(cea, qalys_scaled)
  
  
  qalys_esp <- dt[, lapply(.SD, function(x){
    
    if(length(x) > 1) {
      
      q <- integrate(approxfun(c(1:length(x)), x), 
                     lower = range(c(1:length(x)))[1],
                     upper = range(c(1:length(x)))[2])$value
      
      q <- q * sum(wt_esp)
      
    } else {
      
      q <- x * wt_esp # For individuals with only one year per scenario
      
    }
    
    return(q) 
    
  }), .SDcols = "health_util", by = .(pid, sex, scenario)]
  
  setnames(qalys_esp, "health_util", "qalys_esp")
  
  absorb_dt(cea, qalys_esp)
  
  
  # Calculate Healthcare Costs #
  
  costs_out <- dt[, lapply(.SD, sum), .SDcols = c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
                  by = .(pid, scenario)]
  
  absorb_dt(cea, costs_out)
  
  costs_scl <- dt[, lapply(.SD, function(x){
    
    x <- sum(x * wt)
    
    return(x)
    
  }), .SDcols = c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
  by = .(pid, scenario)]
  
  setnames(costs_scl, c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
                      c("cost_scl", "cost_t2dm_scl", "cost_chd_scl", "cost_stroke_scl"))
  
  absorb_dt(cea, costs_scl)
  
  
  costs_esp <- dt[, lapply(.SD, function(x){
    
    x <- sum(x * wt_esp)
    
    return(x)
    
  }), .SDcols = c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
  by = .(pid, scenario)]
  
  setnames(costs_esp, c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
           c("cost_esp", "cost_t2dm_esp", "cost_chd_esp", "cost_stroke_esp"))
  
  absorb_dt(cea, costs_esp)
  
  cea[, mc := mc_]
  
  if(mean(cea$pid) < 1e8) cea[, pid := pid + 1e8] # safety to ensure right PIDs
  
  return(cea)
}  

cea_res <- rbindlist(out)

cea_res[, `:=`(tot_costs = cost + cost_t2dm + cost_chd + cost_stroke,
               tot_costs_scl = cost_scl + cost_t2dm_scl + cost_chd_scl + cost_stroke_scl,
               tot_costs_esp = cost_esp + cost_t2dm_esp + cost_chd_esp + cost_stroke_esp)]

scenarios <- unique(cea_res$scenario)[scenario != "sc0"] # Exclude baseline scenario

# Compute CEA results per scenario #

cea_diff <- data.table(NULL)

for(i in scenarios){

xx <- cea_res[scenario %in% c("sc0", i)]  
  
xx[, `:=`(incr_qaly = qalys_out - shift(qalys_out),
          incr_cost = tot_costs - shift(tot_costs),
          incr_qaly_scl = qalys_scl - shift(qalys_scl),
          incr_cost_scl = tot_costs_scl - shift(tot_costs_scl),
          incr_qaly_esp = qalys_esp - shift(qalys_esp),
          incr_cost_esp = tot_costs_esp - shift(tot_costs_esp)), keyby = .(mc, pid)]

cea_diff <- rbind(cea_diff, xx)
  
}

cea_agg <- cea_res[, lapply(.SD, mean, na.rm = TRUE),
                   .SDcols = c("incr_qaly", "incr_cost", "incr_qaly_scl", "incr_cost_scl",
                               "incr_qaly_esp", "incr_cost_esp"),
                   keyby = .(mc, scenario)]















