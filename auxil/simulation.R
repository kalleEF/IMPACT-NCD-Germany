#setwd("/home/karlemmert-fees/IMPACT NCD Germany")
source("./global.R")
source("./auxil/scenarios.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml") 
IMPACTncd$del_outputs()$del_logs()
#g <- IMPACTncd$get_causal_structure(print_plot = T)

# Original iterations #
batch_size <- 50
iterations <- 100
first_iteration <- 1
batches <- split(seq(first_iteration, iterations + first_iteration - 1),
                 f = findInterval(seq(first_iteration, iterations + first_iteration - 1),
                                  vec = seq(first_iteration, iterations + first_iteration - 1, batch_size)))


for(i in batches){

 scenario_fn <- function(sp) NULL

 IMPACTncd$
   run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)

}
IMPACTncd$export_summaries(multicore = TRUE, type = c("incd", "prvl", "dis_mrtl", "mrtl"))

#source("./validation_internal/internal_validation_plots.R")

