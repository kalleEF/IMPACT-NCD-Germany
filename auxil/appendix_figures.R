
## Appendix Figures ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
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

prime_impact[, model := "PRIMEtime CE (IMPACT RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


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

prime_original[, model := "PRIMEtime CE (Original RRs)"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


## Combine datasets for plot

dat <- rbind(impact, prime_original, prime_impact)

dat[outcome == "incr_qaly", `:=`(`prvl_rate_2.5%` = `prvl_rate_2.5%` * -1,
                                 `prvl_rate_50.0%` = `prvl_rate_50.0%` * -1,
                                 `prvl_rate_97.5%` = `prvl_rate_97.5%` * -1)]

dat[, outcome := ifelse(outcome == "chd_cpp", "Coronary Heart Disease",
                        ifelse(outcome == "stroke_cpp", "Stroke",
                               ifelse(outcome == "incr_qaly", "Incremental QALYs",
                                      "Type 2 Diabetes")))]

dat[, scenario := ifelse(scenario == "sc1", "Scenario 1",
                         ifelse(scenario == "sc2", "Scenario 2",
                                ifelse(scenario == "sc3", "Scenario 3", "Scenario 4")))]

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
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2)

ggsave(paste0(out_path, "Figure_X_cross_validation.tiff"),
       height = 9, width = 12, dpi = 300)

