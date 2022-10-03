setwd("/home/karlemmert-fees/IMPACT NCD Germany")
source("./global.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
IMPACTncd$del_logs()$del_outputs() 
#g <- IMPACTncd$get_causal_structure(print_plot = T)

batch_size <- 5
iterations <- 30
batches <- split(seq(1, iterations),
                 f = findInterval(seq(1, iterations),
                                  vec = seq(1, iterations, batch_size)))

for(i in batches){

scenario_fn <- function(sp) NULL

IMPACTncd$
  run(i, multicore = TRUE, "Baseline", perc_change_m0 = 0.97)

#scenario_fn <- function(sp) sp$pop[year > 13, bmi_curr_xps := bmi_curr_xps * (1 - 0.1)]
source("./auxil/scenarios.R")

IMPACTncd$
  run(i, multicore = TRUE, "Main Scenario", perc_change_m0 = 0.97)

}
IMPACTncd$export_summaries(multicore = TRUE)

#source("./validation_internal/internal_validation_plots.R")
