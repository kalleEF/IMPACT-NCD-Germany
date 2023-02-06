
##### Health economic parameters ----

# Load packages
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)

# Set global vars
mc_s <- 10000 # Number of MC samples



#### Healthcare costs ----

# Source: Kähm et al. 2020 Diabetic Medicine; Kähm et al. 2018 Diabetes Care #

# Approach: Baseline costs from Kähm 2020.
#           Uncomplicated diabetes cost from Kähm 2018.
#           CHD and Stroke costs for people with diabetes from Kähm 2018.
#           CHD and Stroke costs for people without diabetes using above - uncomplicated diabetes costs
#           from Kähm 2018.
# Assumptions:  - Costs of CHD in people with T2DM are comparable with costs in people without T2DM!
#               - Assumption: Standard error of mean estimate is 2% due to very high sample size and based on SEs in paper!

# TODO: make variable for SD parameter! Necessary?

par_s <- 80 # Paramters

# Inflation factors:

infl_2018 <- 107.3/103.4 # G:\Meine Ablage\PhD\Publications\2021_Diet_simulation_modeling_Germany\Preparation\data\indirect_cost
infl_2020 <- 107.3/105.3

cost_combine <- data.table(NULL)

for(i in 1:mc_s){
  
  ## Ensure replicability #  
  set.seed(log(i) * 42 + 1337) # Seed is parameter-specific!
  
  ## Draw quantiles #
  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
    
  cost_data <- CJ(age_cost =    c("<50", "50-59", "60-69", "70-79", "80+"),
                  sex =         c(1, 0), # 1 = women, 0 = men
                  t2dm_stat =   c(1, 0), # 2 = prevalent year, 1 = incident year, 0 = no disease
                  chd_stat =    c(3, 2, 1, 0),
                  stroke_stat = c(3, 2, 1, 0))
  
  ## Baseline costs (parameter uncertainty!) #
  cost_data[sex == 1, cost := fifelse(age_cost == "<50",   qnorm(quantiles[1], mean = 1019, sd = (1019 - 1002)/1.96),
                              fifelse(age_cost == "50-59", qnorm(quantiles[2], mean = 1483, sd = (1483 - 1465)/1.96),
                              fifelse(age_cost == "60-69", qnorm(quantiles[3], mean = 2140, sd = (2140 - 2120)/1.96),
                              fifelse(age_cost == "70-79", qnorm(quantiles[4], mean = 3156, sd = (3156 - 3127)/1.96),
                                                      qnorm(quantiles[5], mean = 3975, sd = (3975 - 3899)/1.96)))))]
  
  cost_data[sex == 0, cost := fifelse(age_cost == "<50",   qnorm(quantiles[6], mean = 1019, sd = (1019 - 1002)/1.96),
                              fifelse(age_cost == "50-59", qnorm(quantiles[7], mean = 1483, sd = (1483 - 1465)/1.96),
                              fifelse(age_cost == "60-69", qnorm(quantiles[8], mean = 2140, sd = (2140 - 2120)/1.96),
                              fifelse(age_cost == "70-79", qnorm(quantiles[9], mean = 3156, sd = (3156 - 3127)/1.96),
                                                      qnorm(quantiles[10], mean = 3975, sd = (3975 - 3899)/1.96)))))]
  
  cost_data[, cost := cost * infl_2020]
  
  ## Uncomplicated diabetes costs #
  
  cost_data[sex == 1 & t2dm_stat %in% c(1, 2),
            cost_t2dm := fifelse(age_cost == "<50",   qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                         fifelse(age_cost == "50-59", qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                         fifelse(age_cost == "60-69", qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                         fifelse(age_cost == "70-79", qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                 qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat %in% c(1, 2),
            cost_t2dm := fifelse(age_cost == "<50",   qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                         fifelse(age_cost == "50-59", qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                         fifelse(age_cost == "60-69", qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                         fifelse(age_cost == "70-79", qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                 qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[t2dm_stat == 0, cost_t2dm := 0]
  
  cost_data[, cost_t2dm := cost_t2dm * infl_2018]
  
  
  ## Incident CHD and stroke costs for people without diabetes = (- uncomplicated diabetes costs) #
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[21], mean = 8894, sd = 8894 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[22], mean = 11110, sd = 11110 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[23], mean = 8563, sd = 8563 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[24], mean = 8806, sd = 8806 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[25], mean = 8195, sd = 8195 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[26], mean = 7287, sd = 7287 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[27], mean = 9401, sd = 9401 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[28], mean = 9296, sd = 9296 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[29], mean = 9123, sd = 9123 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[30], mean = 8225, sd = 8225 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[31], mean = 24804, sd = 24804 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[32], mean = 15575, sd = 15575 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[33], mean = 15018, sd = 15018 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[34], mean = 14781, sd = 14781 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[35], mean = 11592, sd = 11592 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[36], mean = 13584, sd = 13584 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[37], mean = 13459, sd = 13459 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[38], mean = 14684, sd = 14684 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[39], mean = 14391, sd = 14391 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[40], mean = 12465, sd = 12465 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  ## Incident CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[21], mean = 8894, sd = 8894 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[22], mean = 11110, sd = 11110 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[23], mean = 8563, sd = 8563 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[24], mean = 8806, sd = 8806 * 0.02),
                                                qnorm(quantiles[25], mean = 8195, sd = 8195 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 1,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[26], mean = 7287, sd = 7287 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[27], mean = 9401, sd = 9401 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[28], mean = 9296, sd = 9296 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[29], mean = 9123, sd = 9123 * 0.02),
                                                qnorm(quantiles[30], mean = 8225, sd = 8225 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[31], mean = 24804, sd = 24804 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[32], mean = 15575, sd = 15575 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[33], mean = 15018, sd = 15018 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[34], mean = 14781, sd = 14781 * 0.02),
                                                   qnorm(quantiles[35], mean = 11592, sd = 11592 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 1,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[36], mean = 13584, sd = 13584 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[37], mean = 13459, sd = 13459 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[38], mean = 14684, sd = 14684 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[39], mean = 14391, sd = 14391 * 0.02),
                                                   qnorm(quantiles[40], mean = 12465, sd = 12465 * 0.02)))))]
  
  ## Prevalent CHD and stroke costs for people without diabetes = (- uncomplicated diabetes costs) #
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[41], mean = 7092, sd = 7092 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[42], mean = 4469, sd = 4469 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[43], mean = 3614, sd = 3614 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[44], mean = 4957, sd = 4957 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[45], mean = 4832, sd = 4832 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[46], mean = 2080, sd = 2080 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[47], mean = 4649, sd = 4649 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[48], mean = 4362, sd = 4362 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[49], mean = 3703, sd = 3703 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[50], mean = 4186, sd = 4186 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[51], mean = 34123, sd = 34123 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[52], mean = 14036, sd = 14036 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[53], mean = 10534, sd = 10534 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[54], mean = 9804, sd = 9804 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[55], mean = 5979, sd = 5979 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[56], mean = 7958, sd = 7958 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[57], mean = 7478, sd = 7478 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[58], mean = 10070, sd = 10070 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[59], mean = 9361, sd = 9361 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[60], mean = 6014, sd = 6014 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  ## Prevalent CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[41], mean = 7092, sd = 7092 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[42], mean = 4469, sd = 4469 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[43], mean = 3614, sd = 3614 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[44], mean = 4957, sd = 4957 * 0.02),
                                                qnorm(quantiles[45], mean = 4832, sd = 4832 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 2,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[46], mean = 2080, sd = 2080 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[47], mean = 4649, sd = 4649 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[48], mean = 4362, sd = 4362 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[49], mean = 3703, sd = 3703 * 0.02),
                                                qnorm(quantiles[50], mean = 4186, sd = 4186 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[51], mean = 34123, sd = 34123 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[52], mean = 14036, sd = 14036 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[53], mean = 10534, sd = 10534 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[54], mean = 9804, sd = 9804 * 0.02),
                                                   qnorm(quantiles[55], mean = 5979, sd = 5979 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 2,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[56], mean = 7958, sd = 7958 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[57], mean = 7478, sd = 7478 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[58], mean = 10070, sd = 10070 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[59], mean = 9361, sd = 9361 * 0.02),
                                                   qnorm(quantiles[60], mean = 6014, sd = 6014 * 0.02)))))]
  
  ## Fatal CHD and stroke costs for people with diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 1 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[61], mean = 11263, sd = 11263 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[62], mean = 11193, sd = 11193 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[63], mean = 11177, sd = 11177 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[64], mean = 11295, sd = 11295 * 0.02),
                                                qnorm(quantiles[65], mean = 10880, sd = 10880 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[66], mean = 12765, sd = 12765 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[67], mean = 12874, sd = 12874 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[68], mean = 13031, sd = 13031 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[69], mean = 13220, sd = 13220 * 0.02),
                                                qnorm(quantiles[70], mean = 13021, sd = 13021 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 1 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[71], mean = 10124, sd = 10124 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[72], mean = 10054, sd = 10054 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[73], mean = 10038, sd = 10038 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[74], mean = 10156, sd = 10156 * 0.02),
                                                   qnorm(quantiles[75], mean = 9741, sd = 9741 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 1 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[76], mean = 24209, sd = 24209 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[77], mean = 24319, sd = 24319 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[78], mean = 24475, sd = 24475 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[79], mean = 24665, sd = 24665 * 0.02),
                                                   qnorm(quantiles[80], mean = 24466, sd = 24466 * 0.02)))))]
  
  ## Fatal CHD and stroke costs for people without diabetes #
  
  cost_data[sex == 1 & t2dm_stat == 0 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[61], mean = 11263, sd = 11263 * 0.02) -
                                     qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[62], mean = 11193, sd = 11193 * 0.02) -
                                     qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[63], mean = 11177, sd = 11177 * 0.02) -
                                     qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[64], mean = 11295, sd = 11295 * 0.02) -
                                     qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                   qnorm(quantiles[65], mean = 10880, sd = 10880 * 0.02) -
                                                     qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & stroke_stat == 3,
            cost_stroke := fifelse(age_cost == "<50",   qnorm(quantiles[66], mean = 12765, sd = 12765 * 0.02) -
                                     qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                           fifelse(age_cost == "50-59", qnorm(quantiles[67], mean = 12874, sd = 12874 * 0.02) -
                                     qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                           fifelse(age_cost == "60-69", qnorm(quantiles[68], mean = 13031, sd = 13031 * 0.02) -
                                     qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                           fifelse(age_cost == "70-79", qnorm(quantiles[69], mean = 13220, sd = 13220 * 0.02) -
                                     qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                   qnorm(quantiles[70], mean = 13021, sd = 13021 * 0.02) -
                                                     qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[sex == 1 & t2dm_stat == 0 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[71], mean = 10124, sd = 10124 * 0.02) -
                                  qnorm(quantiles[11], mean = 3001, sd = 3001 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[72], mean = 10054, sd = 10054 * 0.02) -
                                  qnorm(quantiles[12], mean = 2889, sd = 2889 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[73], mean = 10038, sd = 10038 * 0.02) -
                                  qnorm(quantiles[13], mean = 2864, sd = 2864 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[74], mean = 10156, sd = 10156 * 0.02) -
                                  qnorm(quantiles[14], mean = 3052, sd = 3052 * 0.02),
                                                qnorm(quantiles[75], mean = 9741, sd = 9741 * 0.02) -
                                                  qnorm(quantiles[15], mean = 2388, sd = 2388 * 0.02)))))]
  
  cost_data[sex == 0 & t2dm_stat == 0 & chd_stat == 3,
            cost_chd := fifelse(age_cost == "<50",   qnorm(quantiles[76], mean = 24209, sd = 24209 * 0.02) -
                                  qnorm(quantiles[16], mean = 2102, sd = 2102 * 0.02),
                        fifelse(age_cost == "50-59", qnorm(quantiles[77], mean = 24319, sd = 24319 * 0.02) -
                                  qnorm(quantiles[17], mean = 2296, sd = 2296 * 0.02),
                        fifelse(age_cost == "60-69", qnorm(quantiles[78], mean = 24475, sd = 24475 * 0.02) -
                                  qnorm(quantiles[18], mean = 2574, sd = 2574 * 0.02),
                        fifelse(age_cost == "70-79", qnorm(quantiles[79], mean = 24665, sd = 24665 * 0.02) -
                                  qnorm(quantiles[19], mean = 2911, sd = 2911 * 0.02),
                                                qnorm(quantiles[80], mean = 24466, sd = 24466 * 0.02) -
                                                  qnorm(quantiles[20], mean = 2558, sd = 2558 * 0.02)))))]
  
  cost_data[chd_stat == 0, cost_chd := 0]
  cost_data[stroke_stat == 0, cost_stroke := 0]
  
  cost_data[, cost_chd := cost_chd * infl_2018]
  cost_data[, cost_stroke := cost_stroke * infl_2018]
  
  
  cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                   mc = i)]
  
  cost_combine <- rbind(cost_data, cost_combine)
  
}  

write_fst(cost_combine, "./inputs/other_parameters/healthcare_costs.fst")

cost_combine[, rn := .I]
tt <- cost_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/healthcare_costs_indx.fst", 100L)



#### Indirect and productivity costs ----

# Source: Winter et al. 2008, Ulrich et al. 2016, Icks et al. 2020,
#         Icks et al. 2013, DeStatis ~ Verdienststrukturerhebung, Arbeitskosten, Sozialbeiträge der Arbeitnehmer

# Approach: Detailed in Productivity Cost Estimation document.
#           Categories: Premature death cost, Early retirement, Sick leave, T2DM self-management + time cost for health services
# Assumptions:  - ???

# TODO: make variable for SD parameter! Necessary?

par_s <- 80 # Paramters

indir_cost_combine <- data.table(NULL)

age_groups <- c("<25", agegrp_name(min_age = 25, max_age = 65))

for(i in 1:mc_s){
  
  ## Ensure replicability #  
  set.seed(log(i) * 42 + 60 + 1337) # Seed is parameter-specific!
  
  ## Draw quantiles #
  quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
  
  indir_cost_data <- CJ(age_cost =    age_groups,
                        sex =         c(1, 0), # 1 = women, 0 = men
                        t2dm_stat =   c(2, 0), # 2 = prevalent year, 1 = premature death, 0 = no disease
                        stroke_stat = c(2, 0),
                        chd_mort =    c(1, 0),
                        stroke_mort = c(1, 0),
                        other_mort =  c(1, 0))
  
  indir_cost_data <- indir_cost_data[!(chd_mort == 1 & stroke_mort == 1)]
  indir_cost_data <- indir_cost_data[!(chd_mort == 1 & other_mort == 1)]
  indir_cost_data <- indir_cost_data[!(stroke_mort == 1 & other_mort == 1)]
  
  
  
  
  ## Premature death costs (Tabelle 3.3.1 Bruttojahresverdientse in Verdienststrukturerhebung 2018) #
  
  # Women
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "<25" & sex == 1,
                  cost_death := 13067]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "25-29" & sex == 1,
                  cost_death := 28293]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "30-34" & sex == 1,
                  cost_death := 29700]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "35-39" & sex == 1,
                  cost_death := 26133]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "40-44" & sex == 1,
                  cost_death := 25107]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "45-49" & sex == 1,
                  cost_death := 25431]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "50-54" & sex == 1,
                  cost_death := 25939]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "55-59" & sex == 1,
                  cost_death := 25367]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "60-64" & sex == 1,
                  cost_death := 24168]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "65+" & sex == 1,
                  cost_death := 5304]
  
  # Men
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "<25" & sex == 0,
                  cost_death := 14094]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "25-29" & sex == 0,
                  cost_death := 32431]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "30-34" & sex == 0,
                  cost_death := 38100]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "35-39" & sex == 0,
                  cost_death := 41147]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "40-44" & sex == 0,
                  cost_death := 42241]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "45-49" & sex == 0,
                  cost_death := 43841]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "50-54" & sex == 0,
                  cost_death := 45026]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "55-59" & sex == 0,
                  cost_death := 43790]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "60-64" & sex == 0,
                  cost_death := 40812]
  indir_cost_data[(chd_mort == 1 | stroke_mort == 1 | other_mort == 1) &
                    age_cost == "65+" & sex == 0,
                  cost_death := 5400]
  
  # Add fringe benefits (Sozialbeiträge der Arbeitgeber; Tabelle Seite 16 in Arbeits und Lohnkosten 2020)
  
  fringe_rate <- 1 - 48038/62273
  
  indir_cost_data[, cost_death := cost_death/(1-fringe_rate)]
  
  # Inflate to 2022
  
  infl_2018 <- 116.00733/105.8160 # Arbeitskosten index 2022/2018
  
  indir_cost_data[, cost_death := cost_death * infl_2018][is.na(cost_death), cost_death := 0]
  
  
  
  
  ## Early retirement cost
  
  # Type 2 Diabetes (Ulrich et al. 2016) #
  
  # SE from paper: (4103-3024)/1.96 = 550.5102 => 13.41726% of mean estimate => Assumption: Variation of 15%
  
  indir_cost_data[, cost_rtr_t2dm := ifelse(t2dm_stat == 2,
                                            qnorm(quantiles[1], mean = 4103 - 3344, sd = (4103 - 3344) * 0.15),
                                            0)]
  
  infl_ulrich <- 46764/36103 # Same source as Ulrich paper but value for 2021
  infl_2021 <- 116.00733/114.94350 # Additional inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_rtr_t2dm := cost_rtr_t2dm * infl_ulrich * infl_2021]
  
  # Stroke (Winter et al. 2008) #
  
  #Assumption: Gamma distribution due to sample size!
  shape <- 1130**2/1170**2
  scale <- 1170**2/1130
  
  indir_cost_data[, cost_rtr_stroke := ifelse(stroke_stat == 2,
                                              qgamma(quantiles[2], shape = shape, scale = scale),
                                              0)]
  
  infl_2003 <- 116.00733/77.20250 # Inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_rtr_stroke := cost_rtr_stroke * infl_2003]
  
  
  
  
  ## Sick leave cost
  
  # Type 2 Diabetes (Ulrich et al. 2016) #
  
  indir_cost_data[, cost_scklv_t2dm := ifelse(t2dm_stat == 2,
                                              qnorm(quantiles[3], mean = 3344, sd = (3344 - 1995)/1.96),
                                                    0)]
  
  indir_cost_data[, cost_scklv_t2dm := cost_scklv_t2dm * infl_ulrich * infl_2021]
  
  # Stroke (Winter et al. 2008) #
  
  #Assumption: Gamma distribution due to sample size!
  shape <- 130**2/870**2
  scale <- 870**2/130
  
  indir_cost_data[, cost_scklv_stroke := ifelse(stroke_stat == 2,
                                                qgamma(quantiles[4], shape = shape, scale = scale),
                                                0)]
  
  indir_cost_data[, cost_scklv_stroke := cost_scklv_stroke * infl_2003]
  
  
  
  
  ## Type 2 Diabetes Self-Management #
  
  indir_cost_data[, cost_slfmgt_t2dm := ifelse(t2dm_stat == 2,
                                              qnorm(quantiles[5], mean = 2068, sd = (2068 - 1658)/1.96),
                                              0)]

  infl_2014 <- 116.00733/95.36950 # Inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_slfmgt_t2dm := cost_slfmgt_t2dm * infl_2014]
  
  
  ## Health Services Time Cost for People with and without Diabetes #
  
  indir_cost_data[, cost_time_t2dm := ifelse(t2dm_stat == 2,
                                             qnorm(quantiles[6], mean = 2447.1, sd = (2447.1 - 804.5)/1.96),
                                             0)]
  
  indir_cost_data[, cost_time := ifelse(t2dm_stat == 0,
                                        qnorm(quantiles[7], mean = 589.2, sd = (589.2 - 435.8)/1.96),
                                        0)]
  
  infl_2011 <- 116.00733/88.9120 # Additional inflation to 2022 using Arbeitskostenindex
  
  indir_cost_data[, cost_time_t2dm := cost_time_t2dm * infl_2011]
  indir_cost_data[, cost_time := cost_time * infl_2011]
  
  
  indir_cost_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
                         mc = i)]
  
  indir_cost_combine <- rbind(indir_cost_data, indir_cost_combine)
  
}  

indir_cost_combine <- indir_cost_combine[age_cost != "65+"]

write_fst(indir_cost_combine, "./inputs/other_parameters/indirect_costs.fst")

indir_cost_combine[, rn := .I]
tt <- indir_cost_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
write_fst(tt, "./inputs/other_parameters/indirect_costs_indx.fst", 100L)




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
#
#
# ACTUAL UNCERTAINTY IN REGRESSION:
#
# Idea: Like prediction of standard errors! sqrt(t(newdata) %*% vcov(mod) %*% newdata)
#       newdata is predictor values!
#
#
#
# 
# #### OLD/DEPRECEATED: Health utility values ----
# 
# # Source: Laxy et al. 2021 Value in Health #
# 
# util_combine <- data.table(NULL)
# 
# for(i in 1:mc_s){
#  
#   ## Ensure replicability #  
#   set.seed(log(i) * 42/2 + 1337) # Seed is parameter-specific!
#   
#   ## Draw quantiles #
#   quantiles <- runif(par_s, min = 0, max = 1) * 0.9999
#   
#   ## Create data shell #
#   util_data <- CJ(sex =         c(1, 0), # 1 = women, 0 = men
#                   t2dm_stat =   c(1, 0),
#                   chd_stat =    c(3, 2, 1, 0),
#                   stroke_stat = c(3, 2, 1, 0))
#   
#   ## Age and sex utility decrements #
#   util_data[, `:=`(util_incpt = qnorm(quantiles[1], mean = 1.187, sd = 0.019),
#                    util_age = qnorm(quantiles[2], mean = -0.003, sd = 0.0001),
#                    util_sex = sex * qnorm(quantiles[3], mean = -0.029, sd = 0.004))]
#   
#   ## BMI decrement #
#   util_data[, util_bmi := qnorm(quantiles[4], mean = -0.003, sd = 0.0001)]
#   
#   ## Disease-specific utility decrements #
#   # Type 2 Diabetes
#   util_data[, util_disease := fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat == 0,
#                                       qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Only Diabetes
#                               fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat == 0,
#                                       qnorm(quantiles[5], mean = -0.028, sd = 0.014), ## TBD! Diabetes and CHD
#                               fifelse(t2dm_stat == 1 & chd_stat == 0 & stroke_stat >= 1,
#                                       qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
#                                         qnorm(quantiles[5], mean = -0.028, sd = 0.014), # Diabetes and Stroke
#                               fifelse(t2dm_stat == 1 & chd_stat >= 1 & stroke_stat >= 1,
#                                       qnorm(quantiles[6], mean = -0.122, sd = 0.018) +
#                                         qnorm(quantiles[5], mean = -0.028, sd = 0.014), 0))))] # Diabetes, CHD and Stroke
#   
#   # Coronary Heart Disease and Stroke
#   util_data[, util_disease := fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat == 0,
#                                       qnorm(quantiles[7], mean = -0.028, sd = 0.010), # Only CHD
#                               fifelse(t2dm_stat == 0 & chd_stat >= 1 & stroke_stat >= 1,
#                                       qnorm(quantiles[8], mean = -0.070, sd = 0.010) +
#                                         qnorm(quantiles[7], mean = -0.028, sd = 0.010), ## TBD! CHD and Stroke
#                               fifelse(t2dm_stat == 0 & chd_stat == 0 & stroke_stat >= 1,
#                                       qnorm(quantiles[8], mean = -0.070, sd = 0.010), util_disease)))] # Only Stroke 
#   
#   util_data[, `:=`(sex = ifelse(sex == 1, "women", "men"),
#                    mc = i)]
#   
#   util_combine <- rbind(util_data, util_combine)
# 
# }
# 
# write_fst(util_combine, "./inputs/other_parameters/health_utility.fst")
# 
# util_combine[, rn := .I]
# tt <- util_combine[, .(from = min(rn), to = max(rn)), keyby = mc]
# write_fst(tt, "./inputs/other_parameters/health_utility_indx.fst", 100L)

