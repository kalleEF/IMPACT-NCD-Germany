
library(fst)
library(data.table)
library(ggplot2)


cea_data <- CJ(age = seq(20, 100, 1),
               sex = c(1, 0), # 1 = women, 0 = men
               t2dm_prvl = c(1, 0),
               chd_prvl = c(1, 0),
               stroke_prvl = c(1, 0))

#### Health utility values ----
# Source: Laxy et al. 2021 Value in Health #

## Age and sex utility decrements #
cea_data[, util := 1.187 + (age * -0.003) + (sex * -0.003)]

## Disease-specific utility decrements #
# Type 2 Diabetes
cea_data[, util := fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 0,
                           util - 0.028, # Only Diabetes
                           fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 0,
                                   util - 0.028, ## TBD! Diabetes and CHD
                                   fifelse(t2dm_prvl == 1 & chd_prvl == 0 & stroke_prvl == 1,
                                           util - 0.122, # Diabetes and Stroke
                                           fifelse(t2dm_prvl == 1 & chd_prvl == 1 & stroke_prvl == 1,
                                                   util - 0.122, util))))] # Diabetes, CHD and Stroke

# Coronary Heart Disease and Stroke
cea_data[, util := fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 0,
                           util - 0.028, # Only CHD
                           fifelse(t2dm_prvl == 0 & chd_prvl == 1 & stroke_prvl == 1,
                                   util - 0.070, ## TBD! CHD and Stroke
                                   fifelse(t2dm_prvl == 0 & chd_prvl == 0 & stroke_prvl == 1,
                                           util -0.070, util)))] # Only Stroke 













