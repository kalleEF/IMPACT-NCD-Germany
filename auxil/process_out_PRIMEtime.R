

library(openxlsx)
library(data.table)

prime <- read.xlsx("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/02-10-2023_PRIMEtimeGermany_cross_validation.xlsm",
            sheet = "OutputData",
            rows = seq(4, 5010, 1),
            cols = seq(96, 369+95, 1))

prime <- data.table(prime)

prime <- prime[6:5005]

prime <- na.omit(prime)

# Split by intervention

prime_intv_1 <- prime[, 2:93]
prime_intv_2 <- prime[, 94:185]
prime_intv_3 <- prime[, 186:277]
prime_intv_4 <- prime[, 278:369]

# Split by sex

prime_intv_1_male <- prime_intv_1[, 1:30]
prime_intv_1_male[, `:=`(sex = "male", scenario = "sc1", mc = 1:100)]

prime_intv_2_male <- prime_intv_2[, 1:30]
prime_intv_2_male[, `:=`(sex = "male", scenario = "sc2", mc = 1:100)]

prime_intv_3_male <- prime_intv_3[, 1:30]
prime_intv_3_male[, `:=`(sex = "male", scenario = "sc3", mc = 1:100)]

prime_intv_4_male <- prime_intv_4[, 1:30]
prime_intv_4_male[, `:=`(sex = "male", scenario = "sc4", mc = 1:100)]


prime_intv_1_female <- prime_intv_1[, 31:60]
prime_intv_1_female[, `:=`(sex = "female", scenario = "sc1", mc = 1:100)]

prime_intv_2_female <- prime_intv_2[, 31:60]
prime_intv_2_female[, `:=`(sex = "female", scenario = "sc2", mc = 1:100)]

prime_intv_3_female <- prime_intv_3[, 31:60]
prime_intv_3_female[, `:=`(sex = "female", scenario = "sc3", mc = 1:100)]

prime_intv_4_female <- prime_intv_4[, 31:60]
prime_intv_4_female[, `:=`(sex = "female", scenario = "sc4", mc = 1:100)]


# rbind datasets

prime_res <- rbind(prime_intv_1_male, prime_intv_1_female,
                   prime_intv_2_male, prime_intv_2_female,
                   prime_intv_3_male, prime_intv_3_female,
                   prime_intv_4_male, prime_intv_4_female)


# Recode for merge

setnames(prime_res, c("ΔQALY", "ΔCosts.Modelled.diseases", "Ischemic.heart.disease", "Ischemic.stroke", "Diabetes.mellitus.type.2"),
         c("incr_qalys", "incr_costs", "chd_cpp", "stroke_cpp", "diabetes_cpp"))


# Save

fwrite(prime_res, "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/output/PRIMEtime_results.csv")




















