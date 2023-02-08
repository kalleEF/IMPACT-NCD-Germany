#setwd("/home/karlemmert-fees/IMPACT NCD Germany")
source("./global.R")
source("./auxil/scenarios.R")

runif(1)

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml") 
#IMPACTncd$del_outputs()$del_logs()
#g <- IMPACTncd$get_causal_structure(print_plot = T)


# Original iterations #
batch_size <- 1000
iterations <- 1000
batches <- split(seq(1, iterations),
                 f = findInterval(seq(1, iterations),
                                  vec = seq(1, iterations, batch_size)))

j <- c(39, 139, 433, 316, 218, 153, 265, 295,
       348, 213, 117, 370, 326, 297, 362, 380, 267)

for(i in batches){

#i <- as.integer(c(i, i + 1))  
#   
# scenario_fn <- function(sp) NULL
# 
# IMPACTncd$
#   run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
#    
# scenario_fn <- scenario_1_fn
# 
# IMPACTncd$
#   run(i, multicore = TRUE, "sc1", m_zero_trend = -0.03, p_zero_trend = 0)

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

