
### Prepare output tables for manuscript ###

library(data.table)
library(openxlsx)

options(scipen = 999)

# Table 1: Main results table (not stratified)

analysis <- "with_direct_SSB_effects"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, outcome := "cases_prev_post"]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))

case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, outcome := "case_years_prev_post"]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, outcome := "deaths_prev_post"]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_diff_by_year.csv"))
ly <- ly[year == 2043][, year := NULL]
ly[, outcome := "ly"][, disease := "n/a"]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/", analysis, "/tables/incr_qalys_scl_with_direct_SSB_effects_disc_NA.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

table_1 <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly)

setkey(table_1, scenario)

write.xlsx(table_1, "./outputs/manuscript/table_1.xlsx")

# Table X: Main results table stratified by age

# Table X: Main results table stratified by sex

# Table X: Main results table stratified exposure pathways

# Table X: Main results table stratified model ???