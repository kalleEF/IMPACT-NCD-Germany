#setwd("/home/karlemmert-fees/IMPACT NCD Germany")
source("./global.R")
source("./auxil/scenarios.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml") 
IMPACTncd$del_outputs()$del_logs()
#g <- IMPACTncd$get_causal_structure(print_plot = T)

# Original iterations #
batch_size <- 18
iterations <- 100
batches <- split(seq(1, iterations),
                 f = findInterval(seq(1, iterations),
                                  vec = seq(1, iterations, batch_size)))

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

#source("./validation_internal/internal_validation_plots.R")

