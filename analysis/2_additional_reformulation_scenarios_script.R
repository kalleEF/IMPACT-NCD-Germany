
#### Analysis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios.R")
source("./auxil/sensitivity_analyses.R")

# Initiate .Random.seed for safety
runif(1)

# New runs?
new_runs <- FALSE
new_export <- TRUE

# Exports
export_type = "cea"

if(new_runs){
  
  # Create batches for batched simulation
  batch_size <- 10
  iterations <- 500
  first_iteration <- 1
  batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                   f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                    vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))
}

# Logic to ensure correct RR files are loaded
load_RRs <- function(RR){
  
  if(!is.character(RR)) stop("RR needs to be a character vector of IMPACT RR files")
  
  # Load needed files
  
  for(file in unique(RR)){
    
    if(file.exists(paste0("./inputs/RR/", file, ".csvy"))){
      
      print(paste0("RR file ", file, " is already loaded"))
    
      } else if(file.exists(paste0("./inputs/RR/unused/", file, ".csvy"))) {
      
        file.copy(from = paste0("./inputs/RR/unused/", file, ".csvy"),
                  to = paste0("./inputs/RR/", file, ".csvy"))

        file.remove(paste0("./inputs/RR/unused/", file, ".csvy"))

        print(paste0("RR file ", file, " has been loaded"))

      }
  }
  
  # Unload un-needed files
  
  unload_files <- grep(".csvy", outersect(paste0(RR, ".csvy"), list.files("./inputs/RR/")), value = TRUE)
  
  unload_files <- gsub(".csvy", "", unload_files)
  
  for(file in unique(unload_files)){
    
    if(file.exists(paste0("./inputs/RR/", file, ".csvy"))){
      
      file.copy(from = paste0("./inputs/RR/", file, ".csvy"),
                to = paste0("./inputs/RR/unused/", file, ".csvy"))
      
      file.remove(paste0("./inputs/RR/", file, ".csvy"))
      
      print(paste0("RR file ", file, " has been unloaded"))
      
    } else if(file.exists(paste0("./inputs/RR/unused/", file, ".csvy"))) {
      
      print(paste0("RR file ", file, " is already unloaded"))
      
    }
  }
  
}


### Main analysis: Including direct SSB effects ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "ssb~chd", "ssb~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "with_direct_SSB_effects_reform"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

if(new_runs){
  for(i in batches){

    scenario_fn <- scenario_0_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)

    scenario_fn <- scenario_3_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc32", m_zero_trend = -0.03, p_zero_trend = 0)

    scenario_fn <- scenario_4_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc42", m_zero_trend = -0.03, p_zero_trend = 0)

  }
}
if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = export_type)
}

### Alternative analysis: Excluding direct SSB effects ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "without_direct_SSB_effects_reform"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

if(new_runs){
  for(i in batches){

    scenario_fn <- scenario_0_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)

    scenario_fn <- scenario_3_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc32", m_zero_trend = -0.03, p_zero_trend = 0)

    scenario_fn <- scenario_4_fn

    IMPACTncd$
      run(i, multicore = TRUE, "sc42", m_zero_trend = -0.03, p_zero_trend = 0)

  }
}
if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = export_type)
}

### Reformulation sensitivity analyses (no changes in modelled pathways) ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "ssb~chd", "ssb~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "sensitivity_analyses_reform"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

if(new_runs){
  for(i in batches){
    
    scenario_fn <- scenario_0_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
    
    scenario_fn <- scenario_sens_4_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sens_42", m_zero_trend = -0.03, p_zero_trend = 0)
  
    scenario_fn <- scenario_sens_6_fn
    
    IMPACTncd$
      run(i, multicore = TRUE, "sens_6", m_zero_trend = -0.03, p_zero_trend = 0)
      
  }
}
if(new_export){
  IMPACTncd$export_summaries(multicore = TRUE, type = export_type)
}


### Sensitivity analyses: Discounting ----

if(new_export){

  load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
             "ssb~chd", "ssb~t2dm",
             "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

  analysis_name <- "with_direct_SSB_effects_reform"

  IMPACTncd <- Simulation$new("./inputs/sim_design.yaml", analysis_name)

  ## Rename original files with 3% discount rate:
  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  ## Apply 1% discount rate:
  IMPACTncd$design$sim_prm$discount_rate <- 0.01
  IMPACTncd$export_summaries(multicore = TRUE, type = "cea")

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  ## Apply 5% discount rate:
  IMPACTncd$design$sim_prm$discount_rate <- 0.05
  IMPACTncd$export_summaries(multicore = TRUE, type = "cea")

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results",
                          gsub("0.0", "", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  ## Apply 10% discount rate:
  IMPACTncd$design$sim_prm$discount_rate <- 0.10
  IMPACTncd$export_summaries(multicore = TRUE, type = "cea")

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/cea_results",
                          gsub("0.1", "10", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

  file.rename(from = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results.csv.gz"),
              to = paste0(IMPACTncd$design$sim_prm$output_dir, "/", analysis_name, "/summaries/health_economic_results",
                          gsub("0.1", "10", as.character(IMPACTncd$design$sim_prm$discount_rate)), ".csv.gz"))

}



