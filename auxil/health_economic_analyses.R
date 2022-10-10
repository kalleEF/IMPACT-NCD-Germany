
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)

mc_s <- 10000 # Number of MC samples

#### Health utility values ----
# Source: Laxy et al. 2021 Value in Health #

# TODO: Check whether needs to be reestimated to get marginals independent of smoking, education etc.!

## Ensure replicability #  
set.seed(log(mc_s) * 42/2 + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- data.table(quantile = runif(mc_s, min = 0, max = 1) * 0.999,
                        mc = 1:mc_s)

## Create data shell #
util_data <- CJ(age =         seq(20, 100, 1),
                sex =         c(1, 0), # 1 = women, 0 = men
                t2dm_prvl =   c(1, 0),
                chd_prvl =    c(1, 0),
                stroke_prvl = c(1, 0),
                mc =          1:mc_s)

absorb_dt(util_data, quantiles)

## Age and sex utility decrements #
util_data[, util := qnorm(quantile, mean = 1.187, sd = 0.019) +
             (age * qnorm(quantile, mean = -0.003, sd = 0.0001)) +
             (sex * qnorm(quantile, mean = -0.029, sd = 0.004))]

## BMI decrement #
util_data[, util_bmi := qnorm(quantile, mean = -0.003, sd = 0.0001)]

## Disease-specific utility decrements #
# Type 2 Diabetes
util_data[, util_disease := fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 0,
                                    qnorm(quantile, mean = -0.028, sd = 0.014), # Only Diabetes
                            fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 0,
                                    qnorm(quantile, mean = -0.028, sd = 0.014), ## TBD! Diabetes and CHD
                            fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 1,
                                    qnorm(quantile, mean = -0.122, sd = 0.018), # Diabetes and Stroke
                            fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 1,
                                    qnorm(quantile, mean = -0.122, sd = 0.018), 0))))] # Diabetes, CHD and Stroke

# Coronary Heart Disease and Stroke
util_data[, util_disease := fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 0,
                                    qnorm(quantile, mean = -0.028, sd = 0.010), # Only CHD
                            fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 1,
                                    qnorm(quantile, mean = -0.070, sd = 0.010), ## TBD! CHD and Stroke
                            fifelse(t2dm_prvl == 0 & chd_prvl == 0 & stroke_prvl == 1,
                                    qnorm(quantile, mean = -0.070, sd = 0.010), util_disease)))] # Only Stroke 

util_data[, `:=`(sex = ifelse(sex == 1, "women", "men"))]

write_fst(util_data, "./inputs/other_parameters/health_utility.fst")


#### Healthcare costs ----
# Source: Kähm et al. 2020 Diabetic Medicine; Kähm et al. 2018 Diabetes Care #

# Approach: Baseline costs from Kähm 2020.
#           Uncomplicated diabetes cost from Kähm 2018.
#           CHD and Stroke costs for people with diabetes from Kähm 2018.
#           CHD and Stroke costs for people without diabetes using above - uncomplicated diabetes costs
#           from Kähm 2018.

## Ensure replicability #  
set.seed(log(mc_s) * 42 + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- data.table(quantile = runif(mc_s, min = 0, max = 1) * 0.999,
                        mc = 1:mc_s)
  
cost_data <- CJ(age =         c("<50", "50-59", "60-69", "70-79", "80+"),
                sex =         c(1, 0), # 1 = women, 0 = men
                t2dm_prvl =   c(2, 1, 0), # 2 = prevalent year, 1 = incident year, 0 = no disease
                chd_prvl =    c(2, 1, 0),
                stroke_prvl = c(2, 1, 0),
                mc =          1:mc_s)

absorb_dt(cost_data, quantiles)

## Baseline costs (parameter uncertainty!); Discuss with Michael
cost_data[sex == 1, cost := fifelse(age == "<50",   qnorm(quantile, mean = 1019, sd = (1019 - 1002)/1.96),
                            fifelse(age == "50-59", qnorm(quantile, mean = 1483, sd = (1483 - 1465)/1.96),
                            fifelse(age == "60-69", qnorm(quantile, mean = 2140, sd = (2140 - 2120)/1.96),
                            fifelse(age == "70-79", qnorm(quantile, mean = 3156, sd = (3156 - 3127)/1.96),
                                                    qnorm(quantile, mean = 3975, sd = (3975 - 3899)/1.96)))))]

cost_data[sex == 0, cost := fifelse(age == "<50",   qnorm(quantile, mean = 1019, sd = (1019 - 1002)/1.96),
                            fifelse(age == "50-59", qnorm(quantile, mean = 1483, sd = (1483 - 1465)/1.96),
                            fifelse(age == "60-69", qnorm(quantile, mean = 2140, sd = (2140 - 2120)/1.96),
                            fifelse(age == "70-79", qnorm(quantile, mean = 3156, sd = (3156 - 3127)/1.96),
                                                    qnorm(quantile, mean = 3975, sd = (3975 - 3899)/1.96)))))]

cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"))]

write_fst(cost_data, "./inputs/other_parameters/healthcare_costs.fst")


### TESTING FOR COST DATA ###


#se_int <- ((8.023 - 8.012)/1.96)
#se_age50 <- ((-1.129 + 1.149)/1.96)
#se_sex <- ((0.055 - 0.047)/1.96)
#mean <- 8.023 - 1.129 + 0.055
#var <- se_int^2 + se_age^2 + 2 * se_age * 0.5 + 2 * se_sex * 0.5 
#
#shp <- (mean^2)/(var^2) 
#scl <- (var^2)/mean
#df <- as.data.table(exp(rgamma(10000, shape = shp, scale = scl)))
#
##df <- as.data.table(exp(rnorm(10000, mean = mean, sd = var)))
#
#ggplot(df, aes(V1)) + geom_density()
#
#
#exp(qgamma(0.999, shape = shp, scale = scl))







