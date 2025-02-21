
## Appendix Figures ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(scales)
library(viridis)
library(cowplot)

options(scipen = 999)

# ## SET ANALYSIS + IN AND OUT PATHS BEFORE USE ##
if(!Sys.info()[1] == "Windows"){
  
  if(!file.exists(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/appendix/"))){
    dir.create(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/appendix/"))
  }
  
  out_path <- "/media/php-workstation/Storage_1/IMPACT_Storage/outputs/appendix/"
  
} else {
  
  if(!file.exists(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/appendix/"))){
    dir.create(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/appendix/"))
  }
  
  out_path <- "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/appendix/"
  
}

## Figure X: Cross-validation IMPACT vs PRIMEtime including both RR sets ## ----

## IMPACT Results

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact_epi <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario.csv")
} else {
  impact_epi <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario.csv")
}

impact_epi[, `:=`(model = "IMPACT NCD",
                  outcome = ifelse(disease == "diff_t2dm_prvl", "diabetes_cpp",
                                   ifelse(disease == "diff_chd_prvl", "chd_cpp",
                                          ifelse(disease == "diff_stroke_prvl", "stroke_cpp", NA))))][, `:=`(disease = NULL)]

impact_epi <- na.omit(impact_epi)

# CEA Results

if(!Sys.info()[1] == "Windows"){
  impact_cea <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")
} else {
  impact_cea <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")
}

impact_cea[, model := "IMPACT NCD"]

impact_cea <- impact_cea[, lapply(.SD, sum), .SDcols = !c("model", "scenario", "sex", "agegrp", "mc"),
                         by = c("scenario", "mc", "model")]

impact_cea <- impact_cea[, fquantile_byid(incr_qalys_scl, prbl, id = model), keyby = "scenario"]
setnames(impact_cea, c("scenario", "model", percent(prbl, prefix = "prvl_rate_")))

impact_cea <- na.omit(impact_cea)

impact_cea[, outcome := "incr_qaly"]

impact <- rbind(impact_cea, impact_epi)


## PRIMEtime Results with IMPACT RRs

if(!Sys.info()[1] == "Windows"){
  prime_impact <- fread("/home/php-workstation/Schreibtisch/IMPACT/2022_SSB_Tax_Model_Germany/PRIMEtime-CE/PRIMEtime_results_RR_impact.csv")
} else {
  prime_impact <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/output/PRIMEtime_results_RR_impact.csv")
}

prime_impact <- prime_impact[, c("mc", "sex", "scenario", "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")]

prime_impact <- prime_impact[, lapply(.SD, sum), .SDcols = c("incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp"), by = c("mc", "scenario")]

prime_impact <- melt(prime_impact[, c("mc", "scenario",
                                      "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")], id.vars = c("mc", "scenario"))

#prime_impact_mean <- prime_impact[, lapply(.SD, mean), .SDcols = "value", keyby = c("variable", "scenario")]
prime_impact <- prime_impact[, CKutils:::fquantile_byid(value, prbl, id = as.character(variable)), keyby = "scenario"]
setnames(prime_impact, c("scenario", "outcome", scales:::percent(prbl, prefix = "prvl_rate_")))

prime_impact[, model := "PRIMEtime (IMPACT RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


## PRIMEtime Results with original RRs

if(!Sys.info()[1] == "Windows"){
  prime_original <- fread("/home/php-workstation/Schreibtisch/IMPACT/2022_SSB_Tax_Model_Germany/PRIMEtime-CE/PRIMEtime_results_RR_original.csv")
} else {
  prime_original <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/output/PRIMEtime_results_RR_original.csv")
}

prime_original <- prime_original[, c("mc", "sex", "scenario", "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")]

prime_original <- prime_original[, lapply(.SD, sum), .SDcols = c("incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp"), by = c("mc", "scenario")]

prime_original <- melt(prime_original[, c("mc", "scenario",
                                          "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")], id.vars = c("mc", "scenario"))

#prime_original_mean <- prime_original[, lapply(.SD, mean), .SDcols = "value", keyby = c("variable", "scenario")]
prime_original <- prime_original[, CKutils:::fquantile_byid(value, prbl, id = as.character(variable)), keyby = "scenario"]
setnames(prime_original, c("scenario", "outcome", scales:::percent(prbl, prefix = "prvl_rate_")))

prime_original[, model := "PRIMEtime (Original RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


## Combine datasets for plot

dat <- rbind(impact, prime_original, prime_impact)

dat[outcome == "incr_qaly", `:=`(`prvl_rate_2.5%` = `prvl_rate_2.5%` * -1,
                                 `prvl_rate_50.0%` = `prvl_rate_50.0%` * -1,
                                 `prvl_rate_97.5%` = `prvl_rate_97.5%` * -1)]

dat[, outcome := ifelse(outcome == "chd_cpp", "Coronary Heart Disease",
                        ifelse(outcome == "stroke_cpp", "Stroke",
                               ifelse(outcome == "incr_qaly", "Incremental QALYs",
                                      "Type 2 Diabetes")))]

dat[, scenario := ifelse(scenario == "sc1", "Ad-valorem\ntax",
                         ifelse(scenario == "sc2", "Extended\nad-valorem\ntax",
                                ifelse(scenario %in% c("sc32", "sc3"), "Tiered tax", "Scenario 4")))]

dat <- dat[scenario != "Scenario 4"]

## Combined plot for all scenarios

dodge <- position_dodge(width=0.9)

# Panel A: CHD #
sc1 <- ggplot(dat[outcome == "Coronary Heart Disease"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "fixed", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("A - Coronary Heart Disease") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel B: Stroke #
sc2 <- ggplot(dat[outcome == "Stroke"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("B - Stroke") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel C: Type 2 Diabetes #
sc3 <- ggplot(dat[outcome == "Type 2 Diabetes"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("C - Type 2 Diabetes") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel D: QALYs #
sc4 <- ggplot(dat[outcome == "Incremental QALYs"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Quality-adjusted life-years gained") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("D - Incremental QALYs") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.title = element_blank(), legend.text = element_text(size = 12)) +

theme(legend.position = "bottom")

legend <- get_legend(sc4)

sc4 <- sc4 + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2),
                 plot_grid(NULL, legend, ncol = 1), ncol = 1, rel_heights = c(1, 0.05)))

ggsave(paste0(out_path, "Figure_X_cross_validation_RR_sets.tiff"),
       height = 9, width = 16, dpi = 300)



## Figure X: Cross-validation IMPACT vs PRIMEtime including both RR sets stratified by sex ## ----

## IMPACT Results

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact_epi <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario_sex.csv")
} else {
  impact_epi <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario_sex.csv")
}

impact_epi[, `:=`(model = "IMPACT NCD",
                  outcome = ifelse(disease == "diff_t2dm_prvl", "diabetes_cpp",
                                   ifelse(disease == "diff_chd_prvl", "chd_cpp",
                                          ifelse(disease == "diff_stroke_prvl", "stroke_cpp", NA))))][, `:=`(disease = NULL)]

impact_epi <- na.omit(impact_epi)

# CEA Results

if(!Sys.info()[1] == "Windows"){
  impact_cea <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")
} else {
  impact_cea <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")
}

impact_cea[, model := "IMPACT NCD"]

impact_cea <- impact_cea[, lapply(.SD, sum), .SDcols = !c("model", "scenario", "sex", "agegrp", "mc"),
                         by = c("scenario", "mc", "model", "sex")]

impact_cea <- impact_cea[, fquantile_byid(incr_qalys_scl, prbl, id = model), keyby = c("sex", "scenario")]
setnames(impact_cea, c("sex", "scenario", "model", percent(prbl, prefix = "prvl_rate_")))

impact_cea <- na.omit(impact_cea)

impact_cea[, outcome := "incr_qaly"]

impact <- rbind(impact_cea, impact_epi)

impact[, sex := ifelse(sex == "men", "Men", "Women")]


## PRIMEtime Results with IMPACT RRs

if(!Sys.info()[1] == "Windows"){
  prime_impact <- fread("/home/php-workstation/Schreibtisch/IMPACT/2022_SSB_Tax_Model_Germany/PRIMEtime-CE/PRIMEtime_results_RR_impact.csv")
} else {
  prime_impact <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/output/PRIMEtime_results_RR_impact.csv")
}

prime_impact <- prime_impact[, c("mc", "sex", "scenario", "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")]

prime_impact <- prime_impact[, lapply(.SD, sum), .SDcols = c("incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp"), by = c("mc", "scenario", "sex")]

prime_impact <- melt(prime_impact[, c("mc", "scenario", "sex",
                                      "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")], id.vars = c("mc", "scenario", "sex"))

#prime_impact_mean <- prime_impact[, lapply(.SD, mean), .SDcols = "value", keyby = c("variable", "scenario")]
prime_impact <- prime_impact[, CKutils:::fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "scenario")]
setnames(prime_impact, c("sex", "scenario", "outcome", scales:::percent(prbl, prefix = "prvl_rate_")))

prime_impact[, model := "PRIMEtime (IMPACT RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


## PRIMEtime Results with original RRs

if(!Sys.info()[1] == "Windows"){
  prime_original <- fread("/home/php-workstation/Schreibtisch/IMPACT/2022_SSB_Tax_Model_Germany/PRIMEtime-CE/PRIMEtime_results_RR_original.csv")
} else {
  prime_original <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/PRIMEtime-CE/output/PRIMEtime_results_RR_original.csv")
}

prime_original <- prime_original[, c("mc", "sex", "scenario", "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")]

prime_original <- prime_original[, lapply(.SD, sum), .SDcols = c("incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp"), by = c("mc", "scenario", "sex")]

prime_original <- melt(prime_original[, c("mc", "scenario", "sex",
                                          "incr_qalys", "stroke_cpp", "chd_cpp", "diabetes_cpp")], id.vars = c("mc", "scenario", "sex"))

#prime_original_mean <- prime_original[, lapply(.SD, mean), .SDcols = "value", keyby = c("variable", "scenario")]
prime_original <- prime_original[, CKutils:::fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "scenario")]
setnames(prime_original, c("sex", "scenario", "outcome", scales:::percent(prbl, prefix = "prvl_rate_")))

prime_original[, model := "PRIMEtime (Original RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


prime_original[, sex := ifelse(sex == "male", "Men", "Women")]
prime_impact[, sex := ifelse(sex == "male", "Men", "Women")]

## Combine datasets for plot

dat <- rbind(impact, prime_original, prime_impact)

dat[outcome == "incr_qaly", `:=`(`prvl_rate_2.5%` = `prvl_rate_2.5%` * -1,
                                 `prvl_rate_50.0%` = `prvl_rate_50.0%` * -1,
                                 `prvl_rate_97.5%` = `prvl_rate_97.5%` * -1)]

dat[, outcome := ifelse(outcome == "chd_cpp", "Coronary Heart Disease",
                        ifelse(outcome == "stroke_cpp", "Stroke",
                               ifelse(outcome == "incr_qaly", "Incremental QALYs",
                                      "Type 2 Diabetes")))]

dat[, scenario := ifelse(scenario == "sc1", "Ad-valorem\ntax",
                         ifelse(scenario == "sc2", "Extended\nad-valorem\ntax",
                                ifelse(scenario %in% c("sc32", "sc3"), "Tiered tax", "Scenario 4")))]

dat <- dat[scenario != "Scenario 4"]

## Combined plot for all scenarios

dodge <- position_dodge(width=0.9)

facet_labels <- c("Men" = "Male", "Women" = "Female")

# Panel A: CHD #
sc1 <- ggplot(dat[outcome == "Coronary Heart Disease"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  facet_wrap(~ sex, scales = "fixed", ncol = 1, nrow = 4, labeller = as_labeller(facet_labels)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("A - Coronary Heart Disease") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel B: Stroke #
sc2 <- ggplot(dat[outcome == "Stroke"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  facet_wrap(~ sex, scales = "fixed", ncol = 1, nrow = 4, labeller = as_labeller(facet_labels)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("B - Stroke") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel C: Type 2 Diabetes #
sc3 <- ggplot(dat[outcome == "Type 2 Diabetes"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  facet_wrap(~ sex, scales = "fixed", ncol = 1, nrow = 4, labeller = as_labeller(facet_labels)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("C - Type 2 Diabetes") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel D: QALYs #
sc4 <- ggplot(dat[outcome == "Incremental QALYs"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  facet_wrap(~ sex, scales = "fixed", ncol = 1, nrow = 4, labeller = as_labeller(facet_labels)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Quality-adjusted life-years gained") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("D - Incremental QALYs") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(legend.position = "bottom")

legend <- get_legend(sc4)

sc4 <- sc4 + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2),
                 plot_grid(NULL, legend, ncol = 1), ncol = 1, rel_heights = c(1, 0.05)))

ggsave(paste0(out_path, "Figure_X_cross_validation_RR_sets_by_sex.tiff"),
       height = 9, width = 16, dpi = 300)


## Figure X: Cumulative cases prevented/postponed over time by scenario and sex ## ----

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact_epi <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects/tables/cases_prev_post_by_scenario_sex_year.csv")
  impact_epi_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects_reform/tables/cases_prev_post_by_scenario_sex_year.csv")
} else {
  impact_epi <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects/tables/cases_prev_post_by_scenario_sex_year.csv")
  impact_epi_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects_reform/tables/cases_prev_post_by_scenario_sex_year.csv")
}

impact_epi <- rbind(impact_epi[scenario %in% c("sc1", "sc2")],
                    impact_epi_reform[scenario %in% c("sc32", "sc42")])


outcome_names <- c(
  "diff_chd_prvl" = "Coronary heart disease",
  "diff_obesity_prvl" = "Obesity",
  "diff_stroke_prvl" = "Stroke",
  "diff_t2dm_prvl" = "Type 2 diabetes"
)

outcome_labeller <- as_labeller(outcome_names)

sex_names <- c(
  "men" = "Male",
  "women" = "Female"
)

sex_labeller <- as_labeller(sex_names)


# PhD Presentation plots:
# Type 2 Diabetes
ggplot(impact_epi[year == 2043 & disease == "diff_t2dm_prvl" & scenario != "sc42" & scenario != "sc2"],
       aes(x = scenario, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, labeller = sex_labeller) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.3, linewidth = 0.5, col = "black") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed", n.breaks = 6) +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     axis.line.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10),
                     legend.position.inside = c(0.8, 0.3)) +
  theme(legend.position = "inside") +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>"))
ggsave(paste0(out_path, "Diabetes_CPP_pres_phd.tiff"),
       units = "in",
       height = 3.8, width = 8, dpi = 300)

# Coronary Heart Disease
ggplot(impact_epi[year == 2043 & disease == "diff_chd_prvl" & scenario != "sc42"],
       aes(x = scenario, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, labeller = sex_labeller) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.3, linewidth = 0.5, col = "black") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed", n.breaks = 7) +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     axis.line.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10),
                     legend.position.inside = c(0.8, 0.3)) +
  theme(legend.position = "inside") +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>"))
ggsave(paste0(out_path, "CHD_CPP_pres_phd.tiff"),
       units = "in",
       height = 3.8, width = 8, dpi = 300)


# Coronary Heart Disease
ggplot(impact_epi[year == 2043 & disease == "diff_stroke_prvl" & scenario != "sc42"],
       aes(x = scenario, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, labeller = sex_labeller) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.3, linewidth = 0.5, col = "black") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed", n.breaks = 7) +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     axis.line.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10),
                     legend.position.inside = c(0.8, 0.3)) +
  theme(legend.position = "inside") +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>"))
ggsave(paste0(out_path, "Stroke_CPP_pres_phd.tiff"),
       units = "in",
       height = 3.8, width = 8, dpi = 300)

### Paper plots continued ###


sc1 <- ggplot(impact_epi[disease == "diff_t2dm_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
               ymin = `prvl_rate_2.5%`,
               ymax = `prvl_rate_97.5%`,
               fill = scenario,
               col = scenario)) +
  ggtitle("Type 2 Diabetes")
  
sc1 <- sc1 + geom_line() +
             geom_hline(yintercept = 0, linetype = "dashed") +
             geom_ribbon(alpha = 0.3) +
             facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
             scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                                     sc32 = "Tiered tax<sup>&#8225;</sup>")) +
             scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                                    sc32 = "Tiered tax<sup>&#8225;</sup>")) +
             scale_x_continuous(name = "Year") +
             scale_y_continuous(name = "Cumulative cases prevented or postponed") +
             labs(col = "Sex") +
             theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                axis.title = element_text(size = 10),
                                plot.title = element_text(size = 16),
                                strip.text = element_text(size = 10),
                                legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
             theme(legend.position = "none")


sc2 <- ggplot(impact_epi[disease == "diff_chd_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                           ymin = `prvl_rate_2.5%`,
                                                           ymax = `prvl_rate_97.5%`,
                                                           fill = scenario,
                                                           col = scenario)) +
  ggtitle("Coronary heart disease")

sc2 <- sc2 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "none")


sc3 <- ggplot(impact_epi[disease == "diff_stroke_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                           ymin = `prvl_rate_2.5%`,
                                                           ymax = `prvl_rate_97.5%`,
                                                           fill = scenario,
                                                           col = scenario)) +
  ggtitle("Stroke")

sc3 <- sc3 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "none")


sc4 <- ggplot(impact_epi[disease == "diff_obesity_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                           ymin = `prvl_rate_2.5%`,
                                                           ymax = `prvl_rate_97.5%`,
                                                           fill = scenario,
                                                           col = scenario)) +
  ggtitle("Obesity")

sc4 <- sc4 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative cases prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "bottom")

legend <- get_legend(sc4)

sc4 <- sc4 + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2),
                 plot_grid(NULL, legend, ncol = 1), ncol = 1, rel_heights = c(1, 0.05)))

ggsave(paste0(out_path, "Figure_X_cum_cases_prev_sex.tiff"),
       height = 9, width = 16, dpi = 300)



## Figure X: Cumulative case years prevented/postponed over time by scenario and sex ## ----

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact_epi <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects/tables/case_years_prev_post_by_scenario_sex_year.csv")
  impact_epi_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects_reform/tables/case_years_prev_post_by_scenario_sex_year.csv")
} else {
  impact_epi <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects/tables/case_years_prev_post_by_scenario_sex_year.csv")
  impact_epi_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects_reform/tables/case_years_prev_post_by_scenario_sex_year.csv")
}

impact_epi <- rbind(impact_epi[scenario %in% c("sc1", "sc2")],
                    impact_epi_reform[scenario %in% c("sc32", "sc42")])

outcome_names <- c(
  "diff_chd_prvl" = "Coronary heart disease",
  "diff_obesity_prvl" = "Obesity",
  "diff_stroke_prvl" = "Stroke",
  "diff_t2dm_prvl" = "Type 2 diabetes"
)

outcome_labeller <- as_labeller(outcome_names)

sex_names <- c(
  "men" = "Male",
  "women" = "Female"
)


sex_labeller <- as_labeller(sex_names)

# PhD Presentation plots:
# Type 2 Diabetes
ggplot(impact_epi[year == 2043 & disease == "diff_obesity_prvl" & scenario != "sc42" & scenario != "sc2"],
       aes(x = scenario, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, labeller = sex_labeller) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.3, linewidth = 0.5, col = "black") +
  scale_y_continuous(name = "Cumulative cases YEARS prevented or postponed", n.breaks = 6) +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     axis.line.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10),
                     legend.position.inside = c(0.8, 0.3)) +
  theme(legend.position = "inside") +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>"))
ggsave(paste0(out_path, "Obesity_CYPP_pres_phd.tiff"),
       units = "in",
       height = 3.8, width = 8, dpi = 300)

#### Paper plots continued ####

sc1 <- ggplot(impact_epi[disease == "diff_t2dm_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                           ymin = `prvl_rate_2.5%`,
                                                           ymax = `prvl_rate_97.5%`,
                                                           fill = scenario,
                                                           col = scenario)) +
  ggtitle("Type 2 Diabetes")

sc1 <- sc1 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative case years prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "none")


sc2 <- ggplot(impact_epi[disease == "diff_chd_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                          ymin = `prvl_rate_2.5%`,
                                                          ymax = `prvl_rate_97.5%`,
                                                          fill = scenario,
                                                          col = scenario)) +
  ggtitle("Coronary heart disease")

sc2 <- sc2 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative case years prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "none")


sc3 <- ggplot(impact_epi[disease == "diff_stroke_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                             ymin = `prvl_rate_2.5%`,
                                                             ymax = `prvl_rate_97.5%`,
                                                             fill = scenario,
                                                             col = scenario)) +
  ggtitle("Stroke")

sc3 <- sc3 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative case years prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "none")


sc4 <- ggplot(impact_epi[disease == "diff_obesity_prvl" & scenario != "sc42"], aes(x = year, y = `prvl_rate_50.0%`,
                                                              ymin = `prvl_rate_2.5%`,
                                                              ymax = `prvl_rate_97.5%`,
                                                              fill = scenario,
                                                              col = scenario)) +
  ggtitle("Obesity")

sc4 <- sc4 + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ sex, scales = "fixed", labeller = sex_labeller) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                                   sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Cumulative case years prevented or postponed") +
  labs(col = "Sex") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 10),
                     plot.title = element_text(size = 16),
                     strip.text = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 12)) +
  theme(legend.position = "bottom")

legend <- get_legend(sc4)

sc4 <- sc4 + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2),
                 plot_grid(NULL, legend, ncol = 1), ncol = 1, rel_heights = c(1, 0.05)))

ggsave(paste0(out_path, "Figure_X_cum_case_years_prev_sex.tiff"),
       height = 9, width = 16, dpi = 300)


## Figure X: Cumulative costs saved by scenario ## ----

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects/tables/incr_tot_costs_scl_with_direct_SSB_effects_disc_3_by_year.csv")
  impact_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects_reform/tables/incr_tot_costs_scl_with_direct_SSB_effects_reform_disc_3_by_year.csv")
} else {
  impact <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects/tables/incr_tot_costs_scl_with_direct_SSB_effects_disc_3_by_year.csv")
  impact_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects_reform/tables/incr_tot_costs_scl_with_direct_SSB_effects_reform_disc_3_by_year.csv")
}

impact <- rbind(impact[scenario %in% c("sc1", "sc2")],
                impact_reform[scenario %in% c("sc32", "sc42")])

impact[, analysis := NULL]

impact[, `:=`(costs = cumsum(`incr_tot_costs_scl_50.0%`),
              costs_low = cumsum(`incr_tot_costs_scl_2.5%`),
              costs_high = cumsum(`incr_tot_costs_scl_97.5%`)), keyby = c("scenario")]

pnl1 <- ggplot(impact[scenario != "sc42"], aes(x = year, y = costs,
                           ymin = costs_low,
                           ymax = costs_high,
                           fill = scenario,
                           col = scenario)) +
  ggtitle("Societal perspective") +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2023, linetype = "dotdash") +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year", limits = c(2023, 2043)) +
  scale_y_continuous(name = "Cumulative costs saved (in €-millions)", limits = c(-40000000000, 0),
                     labels = function(y) format(y/1000000)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 8),
                     axis.text = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10)) +
  theme(legend.position = "none")

if(!Sys.info()[1] == "Windows"){
  impact <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects/tables/incr_tot_dir_costs_scl_with_direct_SSB_effects_disc_3_by_year.csv")
  impact_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/with_direct_SSB_effects_reform/tables/incr_tot_dir_costs_scl_with_direct_SSB_effects_reform_disc_3_by_year.csv")
} else {
  impact <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects/tables/incr_tot_dir_costs_scl_with_direct_SSB_effects_disc_3_by_year.csv")
  impact_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/with_direct_SSB_effects_reform/tables/incr_tot_dir_costs_scl_with_direct_SSB_effects_reform_disc_3_by_year.csv")
}

impact <- rbind(impact[scenario %in% c("sc1", "sc2")],
                impact_reform[scenario %in% c("sc32", "sc42")])

impact[, analysis := NULL]

impact[, `:=`(costs = cumsum(`incr_tot_dir_costs_scl_50.0%`),
              costs_low = cumsum(`incr_tot_dir_costs_scl_2.5%`),
              costs_high = cumsum(`incr_tot_dir_costs_scl_97.5%`)), keyby = c("scenario")]

pnl2 <- ggplot(impact[scenario != "sc42"], aes(x = year, y = costs,
                           ymin = costs_low,
                           ymax = costs_high,
                           fill = scenario,
                           col = scenario)) +
  ggtitle("Healthcare perspective") +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2023, linetype = "dotdash") +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_color_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                          sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_fill_viridis_d(name = "Scenario", begin = 0, end = 1, labels = c(sc1 = "*Ad-valorem* tax<sup>*</sup>", sc2 = "Extended *ad-valorem* tax<sup>#</sup>",
                                                                         sc32 = "Tiered tax<sup>&#8225;</sup>")) +
  scale_x_continuous(name = "Year", limits = c(2023, 2043)) +
  scale_y_continuous(name = "Cumulative costs saved (in €-millions)", limits = c(-40000000000, 0),
                     labels = function(y) format(y/1000000)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.title = element_text(size = 8),
                     axis.text = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.title = element_blank(), legend.text = element_markdown(size = 10)) +
  theme(legend.position = "bottom")

legend <- get_legend(pnl2)

pnl2 <- pnl2 + theme(legend.position = "none")

ggdraw(plot_grid(plot_grid(pnl1, pnl2, align = "h", ncol = 2),
                 plot_grid(NULL, legend, ncol = 1), ncol = 1, rel_heights = c(1, 0.05)))

ggsave(paste0(out_path, "Figure_X_cum_costs_perspective.tiff"),
       units = "in",
       height = 6, width = 7.5, dpi = 300)



