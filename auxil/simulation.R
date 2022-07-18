source("./global.R")

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

g <- IMPACTncd$get_causal_structure(print_plot = T)

mc_iterations <- 5

for(i in 1:(mc_iterations*5)){
  SynthPop$new(i, design)
}

scenario_fn <- function(sp) NULL

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(7:8, multicore = FALSE, "Baseline")

source("./auxil/scenarios.R")

IMPACTncd$
  run(7:8, multicore = FALSE, "Main Scenario")$
  export_summaries(multicore = TRUE)

# IMPACTncd$export_summaries(multicore = TRUE)

source("./validation_internal/internal_validation_plots.R")
