
#### Anylsis script IMPACT NCD Germany SSB Tax modeling ----

# Load packages
source("./global.R")

# Load scenario and sensitivity analyses functions
source("./auxil/scenarios.R")
source("./auxil/sensitivity_analyses.R")

# Initiate .Random.seed for safety
runif(1)

# Create batches for batched simulation
batch_size <- 8
iterations <- 200
first_iteration <- 1
batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                 f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                  vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))

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

analysis_name <- "with_direct_SSB_effects"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$design$sim_prm$analysis_name <- analysis_name

for(i in batches){
  
  scenario_fn <- function(sp) NULL
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_1_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_2_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_3_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_4_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
  
}

IMPACTncd$export_summaries(multicore = TRUE)


### Policy sensitivity analyses (no changes in modelled pathways) ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "ssb~chd", "ssb~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "policy_sensitivity_analyses"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$design$sim_prm$analysis_name <- analysis_name

for(i in batches){
  
  scenario_fn <- function(sp) NULL
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_sens_1_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sens_1", m_zero_trend = -0.03, p_zero_trend = 0)

  scenario_fn <- scenario_sens_2_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sens_2", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_sens_3_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sens_3", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_sens_4_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sens_4", m_zero_trend = -0.03, p_zero_trend = 0)
    
}

IMPACTncd$export_summaries(multicore = TRUE)


### Other sensitivity analyses (no changes in modelled pathways) ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "ssb~chd", "ssb~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "other_sensitivity"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$design$sim_prm$analysis_name <- analysis_name

for(i in batches){
  
  scenario_fn <- function(sp) NULL
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_1_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_2_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_3_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_4_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
  
}

IMPACTncd$export_summaries(multicore = TRUE)


### Alternative analysis: Excluding direct SSB effects ----

load_RRs(c("bmi~chd", "bmi~obesity", "bmi~stroke", "bmi~t2dm",
           "t2dm_prvl~chd", "t2dm_prvl~nonmodelled", "t2dm_prvl~stroke"))

analysis_name <- "without_direct_SSB_effects"

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$design$sim_prm$analysis_name <- analysis_name

for(i in batches){
  
  scenario_fn <- function(sp) NULL
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_1_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_2_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc2", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_3_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc3", m_zero_trend = -0.03, p_zero_trend = 0)
  
  scenario_fn <- scenario_4_fn
  
  IMPACTncd$
    run(i, multicore = TRUE, "sc4", m_zero_trend = -0.03, p_zero_trend = 0)
  
}

IMPACTncd$export_summaries(multicore = TRUE)






