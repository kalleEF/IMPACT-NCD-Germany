
### Prepare output tables for appendix ###

library(data.table)
library(openxlsx)

options(scipen = 999)

# Table 1: Main results table stratified by sex ----

analysis <- "with_direct_SSB_effects"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario_sex.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))

case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario_sex.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario_sex.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year_sex.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year_sex.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained_by_sex.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3_by_sex.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3_by_sex.csv"))
cost_chd <- cost_chd[, analysis := NULL][, order := 9]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL][, order := 8]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3_by_sex.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3_by_sex.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3_by_sex.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3_by_sex.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3_by_sex.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_1 <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario_sex.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))

case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario_sex.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario_sex.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "sex")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year_sex.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year_sex.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained_by_sex.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3_by_sex.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3_by_sex.csv"))
cost_chd <- cost_chd[, analysis := NULL][, order := 9]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL][, order := 8]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3_by_sex.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3_by_sex.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3_by_sex.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3_by_sex.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3_by_sex.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3_by_sex.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3_by_sex.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_1_reform <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)

table_1_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_1 <- rbind(table_1[scenario %in% c("sc1", "sc2")], table_1_reform)

setkey(table_1, scenario, order)

table_1[outcome %in% grep("_post", table_1$outcome, value = TRUE),
        (grep("%", names(table_1), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order", "sex")]

table_1[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_1), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order", "sex")]

table_1[outcome %in% c("le", "le60"),
        (grep("%", names(table_1), value = TRUE)) := lapply(.SD, round, 2), .SDcols = !c("scenario", "disease",
                                                                                         "outcome", "order", "sex")]

table_1[outcome %in% grep("_cost", table_1$outcome, value = TRUE),
        (grep("%", names(table_1), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order", "sex")]

write.xlsx(table_1, "./outputs/appendix/table_1_main_results_by_sex.xlsx")



# Table 2: Main results table stratified by age group ----

analysis <- "with_direct_SSB_effects"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario_agegrp.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))

case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario_agegrp.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario_age.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained_by_age.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3_by_age.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3_by_age.csv"))
cost_chd <- cost_chd[, analysis := NULL][, order := 9]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL][, order := 8]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3_by_age.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3_by_age.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3_by_age.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3_by_age.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3_by_age.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_2 <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario_agegrp.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))

case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario_agegrp.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario_age.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease", "agegrp")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained_by_age.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3_by_age.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3_by_age.csv"))
cost_chd <- cost_chd[, analysis := NULL][, order := 9]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL][, order := 8]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3_by_age.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3_by_age.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3_by_age.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3_by_age.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3_by_age.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3_by_age.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3_by_age.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_2_reform <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)

table_2_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_2 <- rbind(table_2[scenario %in% c("sc1", "sc2")], table_2_reform)

setkey(table_2, scenario, order)

table_2[outcome %in% grep("_post", table_2$outcome, value = TRUE),
        (grep("%", names(table_2), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order", "agegrp")]

table_2[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_2), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order", "agegrp")]

table_2[outcome %in% grep("_cost", table_2$outcome, value = TRUE),
        (grep("%", names(table_2), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order", "agegrp")]

write.xlsx(table_2, "./outputs/appendix/table_2_main_results_by_age.xlsx")



# Table 3: Main results table stratified by analysis ----

analysis <- "with_direct_SSB_effects"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_3a <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_3.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_3.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_3.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_3.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_3.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_3.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_3.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_3.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_3.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_3.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_3.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_3.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_3.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_3.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_3.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_3a_reform <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                  cost_t2dm, cost_chd, cost_stroke, cost_other,
                  cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                  cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                  cost_healthcare, cost_societal)

table_3a_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_3a <- rbind(table_3a[scenario %in% c("sc1", "sc2")], table_3a_reform)

setkey(table_3a, scenario, order)

table_3a[outcome %in% grep("_post", table_3a$outcome, value = TRUE),
        (grep("%", names(table_3a), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_3a[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_3a), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_3a[outcome %in% c("le", "le60"),
        (grep("%", names(table_3a), value = TRUE)) := lapply(.SD, round, 2), .SDcols = !c("scenario", "disease",
                                                                                         "outcome", "order")]

table_3a[outcome %in% grep("_cost", table_3a$outcome, value = TRUE),
        (grep("%", names(table_3a), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

table_3a[, analysis := analysis]


analysis <- "without_direct_SSB_effects"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_3b <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "without_direct_SSB_effects_reform"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_3b_reform <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                  cost_t2dm, cost_chd, cost_stroke, cost_other,
                  cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                  cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                  cost_healthcare, cost_societal)

table_3b_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_3b <- rbind(table_3b[scenario %in% c("sc1", "sc2")], table_3b_reform)

setkey(table_3b, scenario, order)

table_3b[outcome %in% grep("_post", table_3b$outcome, value = TRUE),
        (grep("%", names(table_3b), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_3b[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_3b), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_3b[outcome %in% c("le", "le60"),
        (grep("%", names(table_3b), value = TRUE)) := lapply(.SD, round, 2), .SDcols = !c("scenario", "disease",
                                                                                         "outcome", "order")]

table_3b[outcome %in% grep("_cost", table_3b$outcome, value = TRUE),
        (grep("%", names(table_3b), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

table_3b[, analysis := analysis]

table_3 <- rbind(table_3a, table_3b)

setkey(table_3, scenario, order, analysis)

write.xlsx(table_3, "./outputs/appendix/table_3_main_results_by_analysis.xlsx")


# Table 4: Sensitivity analyses 1 (not stratified) ----

analysis <- "sensitivity_analyses_1"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_4 <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)

setkey(table_4, scenario, order)

table_4[outcome %in% grep("_post", table_4$outcome, value = TRUE),
        (grep("%", names(table_4), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_4[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_4), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_4[outcome %in% c("le", "le60"),
        (grep("%", names(table_4), value = TRUE)) := lapply(.SD, round, 2), .SDcols = !c("scenario", "disease",
                                                                                         "outcome", "order")]

table_4[outcome %in% grep("_cost", table_4$outcome, value = TRUE),
        (grep("%", names(table_4), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

write.xlsx(table_4, "./outputs/appendix/table_4.xlsx")



# Table 5: Sensitivity analyses 2 (not stratified) ----

analysis <- "sensitivity_analyses_2"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_5 <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "sensitivity_analyses_reform"

cases_prev_post <- fread(paste0("./outputs/", analysis, "/tables/cases_prev_post_by_scenario.csv"))
cases_prev_post[, (grep("%", names(cases_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
cases_prev_post[, outcome := "cases_prev_post"][, order := 1]
setnames(cases_prev_post, grep("prvl_", names(cases_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(cases_prev_post), value = TRUE)))


case_years_prev_post <- fread(paste0("./outputs/", analysis, "/tables/case_years_prev_post_by_scenario.csv"))
case_years_prev_post[, (grep("%", names(case_years_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
case_years_prev_post[, outcome := "case_years_prev_post"][, order := 2]
setnames(case_years_prev_post, grep("prvl_", names(case_years_prev_post), value = TRUE),
         gsub("prvl_rate_", "", grep("prvl_", names(case_years_prev_post), value = TRUE)))

deaths_prev_post <- fread(paste0("./outputs/", analysis, "/tables/deaths_prev_post_by_scenario.csv"))
deaths_prev_post[, (grep("%", names(deaths_prev_post))) := lapply(.SD, `*`, -1), .SDcols = !c("scenario", "disease")]
deaths_prev_post[, outcome := "deaths_prev_post"][, order := 3]
setnames(deaths_prev_post, grep("mrtl_", names(deaths_prev_post), value = TRUE),
         gsub("mrtl_rate_", "", grep("mrtl_", names(deaths_prev_post), value = TRUE)))

le <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_diff_by_year.csv"))
le <- le[year == 2043][, year := NULL]
le[, outcome := "le"][, disease := "n/a"][, order := 6][, scenario := gsub("_diff", "", scenario)]
setnames(le, grep("LE_diff_", names(le), value = TRUE),
         gsub("LE_diff_", "", grep("LE_diff_", names(le), value = TRUE)))

le60 <- fread(paste0("./outputs/", analysis, "/tables/life_expectancy_at_60_diff_by_year.csv"))
le60 <- le60[year == 2043][, year := NULL]
le60[, outcome := "le60"][, disease := "n/a"][, order := 7][, scenario := gsub("_diff", "", scenario)]
setnames(le60, grep("LE60_diff_", names(le60), value = TRUE),
         gsub("LE60_diff_", "", grep("LE60_diff_", names(le60), value = TRUE)))

ly <- fread(paste0("./outputs/", analysis, "/tables/life_years_gained.csv"))
ly[, outcome := "ly"][, disease := "n/a"][, order := 5][, scenario := gsub("_diff", "", scenario)]
setnames(ly, grep("LY_diff_", names(ly), value = TRUE),
         gsub("LY_diff_", "", grep("LY_diff_", names(ly), value = TRUE)))

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_5_reform <- rbind(cases_prev_post, case_years_prev_post, deaths_prev_post, le, le60, ly, qaly,
                        cost_t2dm, cost_chd, cost_stroke, cost_other,
                        cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                        cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                        cost_healthcare, cost_societal)

table_5_reform[, scenario := ifelse(scenario == "sens_42", "sens_4", "sens_6")]

table_5 <- rbind(table_5[scenario %in% c("sens_5")], table_5_reform)

setkey(table_5, scenario, order)

table_5[outcome %in% grep("_post", table_5$outcome, value = TRUE),
        (grep("%", names(table_5), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_5[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_5), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]

table_5[outcome %in% c("le", "le60"),
        (grep("%", names(table_5), value = TRUE)) := lapply(.SD, round, 2), .SDcols = !c("scenario", "disease",
                                                                                         "outcome", "order")]

table_5[outcome %in% grep("_cost", table_5$outcome, value = TRUE),
        (grep("%", names(table_5), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

write.xlsx(table_5, "./outputs/appendix/table_5.xlsx")



# Table 6: Main results table with different discount rates (not stratified) ----

# Discount rate 1% #

analysis <- "with_direct_SSB_effects"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_1.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_1.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_1.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_1.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_1.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_1.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_1.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_1.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_1.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_1.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_1.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_1.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_1.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_1.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_1.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_1 <- rbind(qaly,
                 cost_t2dm, cost_chd, cost_stroke, cost_other,
                 cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                 cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                 cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_1.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_1.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_1.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_1.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_1.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_1.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_1.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_1.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_1.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_1.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_1.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_1.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_1.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_1.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_1.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_1_reform <- rbind(qaly,
                        cost_t2dm, cost_chd, cost_stroke, cost_other,
                        cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                        cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                        cost_healthcare, cost_societal)

table_6_disc_1_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_6_disc_1 <- rbind(table_6_disc_1[scenario %in% c("sc1", "sc2")], table_6_disc_1_reform)

setkey(table_6_disc_1, scenario, order)

table_6_disc_1[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_6_disc_1), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]
table_6_disc_1[outcome %in% grep("_cost", table_6_disc_1$outcome, value = TRUE),
        (grep("%", names(table_6_disc_1), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

table_6_disc_1[, disc := "1%"]


# Discount rate 5% #


analysis <- "with_direct_SSB_effects"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_5.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_5.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_5.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_5.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_5.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_5.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_5.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_5.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_5.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_5.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_5.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_5.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_5.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_5.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_5.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_5 <- rbind(qaly,
                        cost_t2dm, cost_chd, cost_stroke, cost_other,
                        cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                        cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                        cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_5.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_5.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_5.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_5.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_5.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_5.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_5.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_5.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_5.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_5.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_5.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_5.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_5.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_5.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_5.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_5_reform <- rbind(qaly,
                               cost_t2dm, cost_chd, cost_stroke, cost_other,
                               cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                               cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                               cost_healthcare, cost_societal)

table_6_disc_5_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_6_disc_5 <- rbind(table_6_disc_5[scenario %in% c("sc1", "sc2")], table_6_disc_5_reform)

setkey(table_6_disc_5, scenario, order)

table_6_disc_5[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_6_disc_5), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]
table_6_disc_5[outcome %in% grep("_cost", table_6_disc_5$outcome, value = TRUE),
        (grep("%", names(table_6_disc_5), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

table_6_disc_5[, disc := "5%"]


# Discount rate 10% #

analysis <- "with_direct_SSB_effects"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_10.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_10.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_10.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_10.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_10.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_10.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_10.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_10.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_10.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_10.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_10.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_10.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_10.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_10.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_10.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_10 <- rbind(qaly,
                        cost_t2dm, cost_chd, cost_stroke, cost_other,
                        cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                        cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                        cost_healthcare, cost_societal)


analysis <- "with_direct_SSB_effects_reform"

qaly <- fread(paste0("./outputs/",
                     analysis,
                     "/tables/incr_qalys_scl_", analysis, "_disc_10.csv"))
qaly <- qaly[, analysis := NULL]
qaly[, outcome := "qaly"][, disease := "n/a"][, order := 4]
setnames(qaly, grep("incr_qalys_", names(qaly), value = TRUE),
         gsub("incr_qalys_scl_", "", grep("incr_qalys_", names(qaly), value = TRUE)))

cost_chd <- fread(paste0("./outputs/",
                         analysis,
                         "/tables/incr_cost_chd_scl_", analysis, "_disc_10.csv"))
cost_chd <- cost_chd[, analysis := NULL]
cost_chd[, outcome := "disease_cost"][, disease := "diff_chd"][, order := 9]
setnames(cost_chd, grep("incr_cost_", names(cost_chd), value = TRUE),
         gsub("incr_cost_chd_scl_", "", grep("incr_cost_", names(cost_chd), value = TRUE)))

cost_stroke <- fread(paste0("./outputs/",
                            analysis,
                            "/tables/incr_cost_stroke_scl_", analysis, "_disc_10.csv"))
cost_stroke <- cost_stroke[, analysis := NULL]
cost_stroke[, outcome := "disease_cost"][, disease := "diff_stroke"][, order := 10]
setnames(cost_stroke, grep("incr_cost_", names(cost_stroke), value = TRUE),
         gsub("incr_cost_stroke_scl_", "", grep("incr_cost_", names(cost_stroke), value = TRUE)))

cost_t2dm <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_t2dm_scl_", analysis, "_disc_10.csv"))
cost_t2dm <- cost_t2dm[, analysis := NULL]
cost_t2dm[, outcome := "disease_cost"][, disease := "diff_t2dm"][, order := 8]
setnames(cost_t2dm, grep("incr_cost_", names(cost_t2dm), value = TRUE),
         gsub("incr_cost_t2dm_scl_", "", grep("incr_cost_", names(cost_t2dm), value = TRUE)))

cost_other <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_scl_", analysis, "_disc_10.csv"))
cost_other <- cost_other[, analysis := NULL]
cost_other[, outcome := "disease_cost"][, disease := "diff_other"][, order := 11]
setnames(cost_other, grep("incr_cost_", names(cost_other), value = TRUE),
         gsub("incr_cost_scl_", "", grep("incr_cost_", names(cost_other), value = TRUE)))

cost_rtr_t2dm <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_cost_rtr_t2dm_scl_", analysis, "_disc_10.csv"))
cost_rtr_t2dm <- cost_rtr_t2dm[, analysis := NULL]
cost_rtr_t2dm[, outcome := "retire_cost"][, disease := "diff_t2dm"][, order := 12]
setnames(cost_rtr_t2dm, grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE),
         gsub("incr_cost_rtr_t2dm_scl_", "", grep("incr_cost_", names(cost_rtr_t2dm), value = TRUE)))

cost_rtr_stroke <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_rtr_stroke_scl_", analysis, "_disc_10.csv"))
cost_rtr_stroke <- cost_rtr_stroke[, analysis := NULL]
cost_rtr_stroke[, outcome := "retire_cost"][, disease := "diff_stroke"][, order := 14]
setnames(cost_rtr_stroke, grep("incr_cost_", names(cost_rtr_stroke), value = TRUE),
         gsub("incr_cost_rtr_stroke_scl_", "", grep("incr_cost_", names(cost_rtr_stroke), value = TRUE)))

cost_scklv_t2dm <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_cost_scklv_t2dm_scl_", analysis, "_disc_10.csv"))
cost_scklv_t2dm <- cost_scklv_t2dm[, analysis := NULL]
cost_scklv_t2dm[, outcome := "sickleave_cost"][, disease := "diff_t2dm"][, order := 13]
setnames(cost_scklv_t2dm, grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE),
         gsub("incr_cost_scklv_t2dm_scl_", "", grep("incr_cost_", names(cost_scklv_t2dm), value = TRUE)))

cost_scklv_stroke <- fread(paste0("./outputs/",
                                  analysis,
                                  "/tables/incr_cost_scklv_stroke_scl_", analysis, "_disc_10.csv"))
cost_scklv_stroke <- cost_scklv_stroke[, analysis := NULL]
cost_scklv_stroke[, outcome := "sickleave_cost"][, disease := "diff_stroke"][, order := 15]
setnames(cost_scklv_stroke, grep("incr_cost_", names(cost_scklv_stroke), value = TRUE),
         gsub("incr_cost_scklv_stroke_scl_", "", grep("incr_cost_", names(cost_scklv_stroke), value = TRUE)))

cost_death <- fread(paste0("./outputs/",
                           analysis,
                           "/tables/incr_cost_death_scl_", analysis, "_disc_10.csv"))
cost_death <- cost_death[, analysis := NULL]
cost_death[, outcome := "death_cost"][, disease := "diff_all"][, order := 16]
setnames(cost_death, grep("incr_cost_", names(cost_death), value = TRUE),
         gsub("incr_cost_death_scl_", "", grep("incr_cost_", names(cost_death), value = TRUE)))

cost_slfmgt_t2dm <- fread(paste0("./outputs/",
                                 analysis,
                                 "/tables/incr_cost_slfmgt_t2dm_scl_", analysis, "_disc_10.csv"))
cost_slfmgt_t2dm <- cost_slfmgt_t2dm[, analysis := NULL]
cost_slfmgt_t2dm[, outcome := "time_slfmgt_cost"][, disease := "diff_t2dm"][, order := 17]
setnames(cost_slfmgt_t2dm, grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE),
         gsub("incr_cost_slfmgt_t2dm_scl_", "", grep("incr_cost_", names(cost_slfmgt_t2dm), value = TRUE)))

cost_time_t2dm <- fread(paste0("./outputs/",
                               analysis,
                               "/tables/incr_cost_time_t2dm_scl_", analysis, "_disc_10.csv"))
cost_time_t2dm <- cost_time_t2dm[, analysis := NULL]
cost_time_t2dm[, outcome := "time_healthservice_cost"][, disease := "diff_t2dm"][, order := 18]
setnames(cost_time_t2dm, grep("incr_cost_", names(cost_time_t2dm), value = TRUE),
         gsub("incr_cost_time_t2dm_scl_", "", grep("incr_cost_", names(cost_time_t2dm), value = TRUE)))

cost_time <- fread(paste0("./outputs/",
                          analysis,
                          "/tables/incr_cost_time_scl_", analysis, "_disc_10.csv"))
cost_time <- cost_time[, analysis := NULL]
cost_time[, outcome := "time_healthservice_cost"][, disease := "diff_other"][, order := 19]
setnames(cost_time, grep("incr_cost_", names(cost_time), value = TRUE),
         gsub("incr_cost_time_scl_", "", grep("incr_cost_", names(cost_time), value = TRUE)))

cost_healthcare <- fread(paste0("./outputs/",
                                analysis,
                                "/tables/incr_tot_dir_costs_scl_", analysis, "_disc_10.csv"))
cost_healthcare <- cost_healthcare[, analysis := NULL]
cost_healthcare[, outcome := "healthcare_perspective_cost"][, disease := "diff_all"][, order := 20]
setnames(cost_healthcare, grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE),
         gsub("incr_tot_dir_costs_scl_", "", grep("incr_tot_dir_costs_", names(cost_healthcare), value = TRUE)))

cost_societal <- fread(paste0("./outputs/",
                              analysis,
                              "/tables/incr_tot_costs_scl_", analysis, "_disc_10.csv"))
cost_societal <- cost_societal[, analysis := NULL]
cost_societal[, outcome := "societal_perspective_cost"][, disease := "diff_all"][, order := 21]
setnames(cost_societal, grep("incr_tot_costs_", names(cost_societal), value = TRUE),
         gsub("incr_tot_costs_scl_", "", grep("incr_tot_costs_", names(cost_societal), value = TRUE)))



table_6_disc_10_reform <- rbind(qaly,
                               cost_t2dm, cost_chd, cost_stroke, cost_other,
                               cost_rtr_t2dm, cost_scklv_t2dm, cost_rtr_stroke, cost_scklv_stroke,
                               cost_death, cost_slfmgt_t2dm, cost_time_t2dm, cost_time,
                               cost_healthcare, cost_societal)

table_6_disc_10_reform[, scenario := ifelse(scenario == "sc32", "sc3", "sc4")]

table_6_disc_10 <- rbind(table_6_disc_10[scenario %in% c("sc1", "sc2")], table_6_disc_10_reform)

setkey(table_6_disc_10, scenario, order)

table_6_disc_10[outcome %in% c("ly", "qaly"),
        (grep("%", names(table_6_disc_10), value = TRUE)) := lapply(.SD, round, -2), .SDcols = !c("scenario", "disease",
                                                                                          "outcome", "order")]
table_6_disc_10[outcome %in% grep("_cost", table_6_disc_10$outcome, value = TRUE),
        (grep("%", names(table_6_disc_10), value = TRUE)) := lapply(.SD, function(x){x <- round(x/1000000, 0)}),
        .SDcols = !c("scenario", "disease", "outcome", "order")]

table_6_disc_10[, disc := "6_10%"]


# Combine to one table #

table_6 <- rbind(table_6_disc_1, table_6_disc_5, table_6_disc_10)

setkey(table_6, scenario, disc, order)


write.xlsx(table_6, "./outputs/appendix/table_6.xlsx")
