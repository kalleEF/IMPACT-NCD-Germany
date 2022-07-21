source("./global.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

g <- IMPACTncd$get_causal_structure(print_plot = T)

scenario_fn <- function(sp) NULL

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(3:4, multicore = FALSE, "Baseline", perc_change_m0 = 0.97)

source("./auxil/scenarios.R")

IMPACTncd$
  run(3:4, multicore = FALSE, "Main Scenario", perc_change_m0 = 0.97)$
  export_summaries(multicore = TRUE)

# IMPACTncd$export_summaries(multicore = TRUE)

source("./validation_internal/internal_validation_plots.R")
