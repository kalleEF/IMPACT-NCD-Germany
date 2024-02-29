

#### Comparison of DISMOD incidence and model output ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
theme_set(new = theme_hc())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

analysis <- "with_direct_SSB_effects"

## Incidence ## ----

tt <- fread(paste0("./outputs/", analysis, "/summaries/incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
# d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

d <- d[year == 2013 & scenario == "sc0" & variable %in% c("t2dm_prvl", "chd_prvl", "stroke_prvl")]

d[, disease := gsub("_prvl", "", variable)]

# e <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_final.csv", stringsAsFactors = TRUE,
#                             select = c("age", "sex", "disease", "out_incidence_rates",
#                                        "out_prevalence_rates", "out_case_fatality_rates",
#                                        "out_duration_years"))

e <- fst::read_fst("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_l.fst", as.data.table = TRUE)
e <- e[, c("age", "sex", "mc", "value_chd_incidence_rates", "value_stroke_incidence_rates", "value_diabetes_incidence_rates")]



to_agegrp(e, max_age = 100, to_factor = FALSE)

e <- melt(e[mc != 0], id.vars = c("sex", "age", "agegrp", "mc"))

#e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "agegrp")]
#setnames(e, c("sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))

e[, disease := gsub("_incidence_rates", "", variable)]
e[, disease := gsub("value_", "", disease)]
e[disease == "diabetes", disease := "t2dm"]

model_ages <- unique(d$agegrp)

e[, sex := ifelse(sex == "male", "Men", "Women")][, type := "Input"]
d[, sex := ifelse(sex == "men", "Men", "Women")][, type := "Output"]

d <- d[, c("mc", "agegrp", "sex", "disease", "value", "type")]
e <- e[, c("mc", "agegrp", "sex", "disease", "value", "type")]

de <- rbind(d, e)

for(i in unique(de$disease)){


  if(i == "chd"){dis_nam <- "CHD"}
  if(i == "stroke"){dis_nam <- "Stroke"}
  if(i == "t2dm"){dis_nam <- "Type 2 Diabetes"}
  
  facet_labels <- c(`Men` = "Male",
                    `Women` = "Female")
  
  ggplot(de[disease == i], aes(x = agegrp, y = value, fill = type, col = type
  )) +
  facet_wrap(~ sex, labeller = as_labeller(facet_labels)) +
  geom_point(alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.4, jitter.height = 0, dodge.width = 1.0)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975), alpha = 0.3,
              position = position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width = 1.0)) +
  scale_x_discrete(name = "Age group (years)") +
  scale_y_continuous(name = "Incidence rate") +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_color_viridis_d(begin = 0.1, end = 0.7) +
  #ggtitle(paste("Comparison of", dis_nam, "input and output incidence rates")) +
  expand_limits(y = 0) +
  theme(legend.title = element_blank(), legend.position = "bottom",
        axis.title.y = element_text(angle = 90))

cowplot::ggsave2(paste0("validation_i_o_incidence_", i, "_", analysis, ".png"), width = 16, height = 9, units = "cm", 
                 scale = 2, dpi = 300, path = "./validation_internal/input_output/")

}



## Prevalence ## ----

tt <- fread(paste0("./outputs/", analysis, "/summaries/prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
# d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

d <- d[year == 2013 & scenario == "sc0" & variable %in% c("t2dm_prvl", "chd_prvl", "stroke_prvl")]

d[, disease := gsub("_prvl", "", variable)]

# e <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_final.csv", stringsAsFactors = TRUE,
#                             select = c("age", "sex", "disease", "out_incidence_rates",
#                                        "out_prevalence_rates", "out_case_fatality_rates",
#                                        "out_duration_years"))

e <- fst::read_fst("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_l.fst", as.data.table = TRUE)
e <- e[, c("age", "sex", "mc", "value_chd_prevalence_rates", "value_stroke_prevalence_rates", "value_diabetes_prevalence_rates")]



to_agegrp(e, max_age = 100, to_factor = F)

e <- melt(e[mc != 0], id.vars = c("sex", "age", "agegrp", "mc"))
# 
# e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "agegrp")]
# setnames(e, c("sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))

e[, disease := gsub("_prevalence_rates", "", variable)]
e[, disease := gsub("value_", "", disease)]
e[disease == "diabetes", disease := "t2dm"]

model_ages <- unique(d$agegrp)

e[, sex := ifelse(sex == "male", "Men", "Women")][, type := "Input"]
d[, sex := ifelse(sex == "men", "Men", "Women")][, type := "Output"]

d <- d[, c("mc", "agegrp", "sex", "disease", "value", "type")]
e <- e[, c("mc", "agegrp", "sex", "disease", "value", "type")]

de <- rbind(d, e)

for(i in unique(d$disease)){
  
  
  if(i == "chd"){dis_nam <- "CHD"}
  if(i == "stroke"){dis_nam <- "Stroke"}
  if(i == "t2dm"){dis_nam <- "Type 2 Diabetes"}
  
  ggplot(de[disease == i], aes(x = agegrp, y = value, fill = type, col = type
  )) +
    facet_wrap(~ sex, labeller = as_labeller(facet_labels)) +
    geom_point(alpha = 0.2,
               position = position_jitterdodge(jitter.width = 0.4, jitter.height = 0, dodge.width = 1.0)) +
    geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975), alpha = 0.3,
                position = position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width = 1.0)) +
    scale_x_discrete(name = "Age group (years)") +
    scale_y_continuous(name = "Prevalence rate") +
    scale_fill_viridis_d(begin = 0.1, end = 0.7) +
    scale_color_viridis_d(begin = 0.1, end = 0.7) +
    #ggtitle(paste("Comparison of", dis_nam, "input and output prevalence rates")) +
    expand_limits(y = 0) +
    theme(legend.title = element_blank(), legend.position = "bottom",
          axis.title.y = element_text(angle = 90))
  
  cowplot::ggsave2(paste0("validation_i_o_prevalence_", i, "_", analysis, ".png"), width = 16, height = 9, units = "cm", 
                   scale = 2, dpi = 300, path = "./validation_internal/input_output/")
  
}
