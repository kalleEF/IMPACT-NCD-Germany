
library(fst)
library(data.table)
library(ggplot2)

mc_s <- 10000 # Number of MC samples

n_s <- 8 # Number of utility parameters

util_data <- data.table(NULL)

for(i in 1:mc_s){

  ## Ensure replicability #  
  set.seed(i + 1337)
  
  ## Draw quantiles #
  quantiles <- runif(n_s, min = 0, max = 1) * 0.999

  #### Health utility values ----
  # Source: Laxy et al. 2021 Value in Health #
  
  xx <- CJ(age = seq(20, 100, 1),
           sex = c(1, 0), # 1 = women, 0 = men
           t2dm_prvl = c(1, 0),
           chd_prvl = c(1, 0),
           stroke_prvl = c(1, 0))
  
  ## Age and sex utility decrements #
  xx[, util := qnorm(quantiles[1], mean = 1.187, sd = 0.019) +
              (age * qnorm(quantiles[2], mean = -0.003, sd = 0.0001)) +
              (sex * qnorm(quantiles[3], mean = -0.029, sd = 0.004))]
  
  ## BMI decrement #
  xx[, util_bmi := qnorm(quantiles[4], mean = -0.003, sd = 0.0001)]
  
  ## Disease-specific utility decrements #
  # Type 2 Diabetes
  xx[, util_disease := fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 0,
                               qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Only Diabetes
                       fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 0,
                               qnorm(quantiles[5], mean = -0.028, sd = 0.014), ## TBD! Diabetes and CHD
                       fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 1,
                               qnorm(quantiles[6], mean = -0.122, sd = 0.018), # Diabetes and Stroke
                       fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 1,
                               qnorm(quantiles[6], mean = -0.122, sd = 0.018), 0))))] # Diabetes, CHD and Stroke
  
  # Coronary Heart Disease and Stroke
  xx[, util_disease := fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 0,
                               qnorm(quantiles[7], mean = -0.028, sd = 0.010), # Only CHD
                       fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 1,
                               qnorm(quantiles[8], mean = -0.070, sd = 0.010), ## TBD! CHD and Stroke
                       fifelse(t2dm_prvl == 0 & chd_prvl == 0 & stroke_prvl == 1,
                               qnorm(quantiles[8], mean = -0.070, sd = 0.010), util_disease)))] # Only Stroke 
  
  xx[, `:=`(sex = ifelse(sex == 1, "women", "men"),
            mc = i)]
  
  util_data <- rbind(xx, util_data)
  
}

write_fst(util_data, "./inputs/other_parameters/health_utility.fst")







