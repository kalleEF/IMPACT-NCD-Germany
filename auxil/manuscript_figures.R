
## Manuscript Figures ## ----

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
  
  if(!file.exists(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/manuscript/"))){
    dir.create(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/manuscript/"))
  }

  out_path <- "/media/php-workstation/Storage_1/IMPACT_Storage/outputs/manuscript/"

} else {
  
  if(!file.exists(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/manuscript/"))){
    dir.create(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/manuscript/"))
  }
  
  out_path <- "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/manuscript/"
  
}

## Figure 2: Cost-effectiveness ## ----

if(!Sys.info()[1] == "Windows"){
  cea_wo <- fread(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/", "without_direct_SSB_effects", "/summaries/cea_results.csv.gz"))
  cea_wo[, analysis := "Only BMI-mediated effects"]
  cea_w <- fread(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/", "with_direct_SSB_effects", "/summaries/cea_results3.csv.gz"))
  cea_w[, analysis := "All exposure pathways"]
  
  # Include new scenario 3
  cea_wo_reform <- fread(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/", "without_direct_SSB_effects_reform", "/summaries/cea_results.csv.gz"))
  cea_wo_reform[, analysis := "Only BMI-mediated effects"]
  cea_w_reform <- fread(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/", "with_direct_SSB_effects_reform", "/summaries/cea_results3.csv.gz"))
  cea_w_reform[, analysis := "All exposure pathways"]
  
} else {
  cea_wo <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                         "without_direct_SSB_effects", "/summaries/cea_results.csv.gz"))
  cea_wo[, analysis := "Only BMI-mediated effects"]
  cea_w <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                        "with_direct_SSB_effects", "/summaries/cea_results3.csv.gz"))
  cea_w[, analysis := "All exposure pathways"]
  
  # Include new scenario 3
  cea_wo_reform <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                         "without_direct_SSB_effects_reform", "/summaries/cea_results.csv.gz"))
  cea_wo_reform[, analysis := "Only BMI-mediated effects"]
  cea_w_reform <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                        "with_direct_SSB_effects_reform", "/summaries/cea_results3.csv.gz"))
  cea_w_reform[, analysis := "All exposure pathways"]
}

cea <- rbind(cea_w[!(scenario %in% c("sc3", "sc4"))],
             cea_wo[!(scenario %in% c("sc3", "sc4"))],
             cea_w_reform[scenario != "sc0"],
             cea_wo_reform[scenario != "sc0"])

cea_agg <- cea[, lapply(.SD, sum), .SDcols = !c("analysis", "scenario", "sex", "agegrp", "mc"),
               by = c("scenario", "mc", "analysis")]

#TODO: Eventually extend by error bars!
cea_point <- data.table(x = cea_agg[scenario != "sc0", mean(incr_qalys_scl, na.rm = T), by = .(scenario, analysis)],
                        y = cea_agg[scenario != "sc0", mean(incr_tot_costs_scl, na.rm = T), by = .(scenario, analysis)],
                        xmin = cea_agg[scenario != "sc0", quantile(incr_qalys_scl, probs = 0.025, na.rm = T), by = .(scenario, analysis)],
                        xmax = cea_agg[scenario != "sc0", quantile(incr_qalys_scl, probs = 0.975, na.rm = T), by = .(scenario, analysis)],
                        ymin = cea_agg[scenario != "sc0", quantile(incr_tot_costs_scl, probs = 0.025, na.rm = T), by = .(scenario, analysis)],
                        ymax = cea_agg[scenario != "sc0", quantile(incr_tot_costs_scl, probs = 0.975, na.rm = T), by = .(scenario, analysis)])
                        
cea_point[, `:=`(scenario = x.scenario, y.scenario = NULL, ymin.scenario = NULL, ymax.scenario = NULL, xmin.scenario = NULL, xmax.scenario = NULL,
                 analysis = x.analysis, y.analysis = NULL, ymin.analysis = NULL, ymax.analysis = NULL, xmin.analysis = NULL, xmax.analysis = NULL,
                 x.analysis = NULL, x.scenario = NULL,
                 mean_qaly = x.V1, mean_cost = y.V1, x.V1 = NULL, y.V1 = NULL,
                 ymin_cost = ymin.V1, ymax_cost = ymax.V1, ymin.V1 = NULL, ymax.V1 = NULL,
                 xmin_qaly = xmin.V1, xmax_qaly = xmax.V1, xmin.V1 = NULL, xmax.V1 = NULL)]

ggplot(cea_agg[scenario %in% c("sc1", "sc2", "sc32")], aes(x = incr_qalys_scl,
                                       y = incr_tot_costs_scl,
                                       col = scenario)) +
  facet_wrap(~ analysis) +
  geom_point(shape = 16, alpha = 0.5, size = 3) +
  #geom_point(data = cea_point, aes(x = mean_qaly, y = mean_cost, col = scenario), shape = 4, size = 5, stroke = 1.5) +
  geom_errorbar(data = cea_point[scenario %in% c("sc1", "sc2", "sc32")], aes(x = mean_qaly, y = mean_cost, ymin = ymin_cost, ymax = ymax_cost), inherit.aes = FALSE) +
  geom_errorbar(data = cea_point[scenario %in% c("sc1", "sc2", "sc32")], aes(x = mean_qaly, y = mean_cost, xmin = xmin_qaly, xmax = xmax_qaly), inherit.aes = FALSE) +
  geom_point(data = cea_point[scenario %in% c("sc1", "sc2", "sc32")], aes(x = mean_qaly, y = mean_cost), shape = 1, size = 2, col = "black") +
  #geom_point(data = cea_point, aes(x = mean_qaly, y = mean_cost), shape = 4, size = 4, col = "black") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  #expand_limits(x = -8e4, y = 2e9) +
  scale_y_continuous(name = "Incremental costs (in €-billions)", c(seq(0,80000000000,5000000000)*-1),
                     labels = function(y) format(y/1000000000)) +
  scale_x_continuous(name = "Incremental QALYs (in thousands)", c(seq(-1000000,500000,50000)), labels = function(y) format(y/1000)) +
  scale_color_viridis_d(name = "Scenario", option = "viridis",
                       labels = c("20% ad-valorem tax on SSBs", "20% ad-valorem tax on SSBs & fruit juice",
                                  "Tiered tax with 30% reformulation")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), strip.text.x = element_text(size = 15),
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=13), axis.title=element_text(size=15),
                     legend.position = c(0.8,0.3),
                     legend.title = element_text(size = 15), legend.text = element_text(size = 13))


ggsave(paste0(out_path, "Figure_1_cost_effectiveness_plane.tiff"),
       height = 9, width = 14, dpi = 300)




## Figure 3: Cross-validation IMPACT vs PRIMEtime ## ----

## IMPACT Results

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)

# Epi Results ENTER CORRECT ANALYSIS IN PATH!!!!!
if(!Sys.info()[1] == "Windows"){
  impact_epi <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario.csv")

  impact_epi_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects_reform/tables/cases_prev_post_by_scenario.csv")
} else {
  impact_epi <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/tables/cases_prev_post_by_scenario.csv")

  impact_epi_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects_reform/tables/cases_prev_post_by_scenario.csv")
}

#fwrite(impact_epi, "G:/Meine Ablage/PhD/Presentations/2022_EUPHA/impact_epi_results.csv", sep = ";", dec = ".")

impact_epi <- rbind(impact_epi[!(scenario %in% c("sc3", "sc4"))],
                    impact_epi_reform[scenario != "sc0"])

impact_epi[, `:=`(model = "IMPACT NCD",
                  outcome = ifelse(disease == "diff_t2dm_prvl", "diabetes_cpp",
                                   ifelse(disease == "diff_chd_prvl", "chd_cpp",
                                          ifelse(disease == "diff_stroke_prvl", "stroke_cpp", NA))))][, `:=`(disease = NULL)]

impact_epi <- na.omit(impact_epi)

# CEA Results

if(!Sys.info()[1] == "Windows"){
  impact_cea <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")

  impact_cea_reform <- fread("/media/php-workstation/Storage_1/IMPACT_Storage/outputs/without_direct_SSB_effects_reform/summaries/cea_results.csv.gz")
} else {
  impact_cea <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects/summaries/cea_results.csv.gz")

  impact_cea_reform <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/without_direct_SSB_effects_reform/summaries/cea_results.csv.gz")
}

impact_cea <- rbind(impact_cea[!(scenario %in% c("sc3", "sc4"))],
                    impact_cea_reform[scenario != "sc0"])

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

prime_impact[, model := "PRIMEtime CE"][, outcome := ifelse(outcome == "incr_qalys", "incr_qaly", outcome)]


## Combine datasets for plot

dat <- rbind(impact, prime_impact)

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

setkey(dat, scenario, outcome)

openxlsx::write.xlsx(dat, "./outputs/appendix/table_7.xlsx")

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

ggsave(paste0(out_path, "Figure_2_cross_validation.tiff"),
       height = 9, width = 14, dpi = 300)



