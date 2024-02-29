
#### Validation of IMPACT NCD results with external epi sources ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)
library(cowplot)

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9) # Quantiles for uncertainty of results

theme_set(new = theme_hc())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

external_path <- "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/data/disease_data/"

if(!file.exists(paste0("./validation_internal/external_sources/"))){
  dir.create(paste0("./validation_internal/external_sources/"))
}

analysis <- "with_direct_SSB_effects"

tt <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                   analysis, "/summaries/", "prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "year", "sex", "agegrp", "scenario")

sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario)))) 

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))


tt <- fread(paste0("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT-NCD-Germany/outputs/",
                   analysis, "/summaries/", "incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "year", "sex", "agegrp", "scenario")

sc_n <- na.omit(as.numeric(gsub("[^1-9]+", "", unique(tt$scenario)))) 

e <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
e <- melt(e, id.vars = outstrata)
e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(e, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))


## Coronary Heart Disease ----

# Prevalence

impact_chd_prev <- d[
  scenario == "sc0" & disease == "chd_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_chd_prev <- fread(paste0(external_path, "chd/ger_chd_data_combined.csv"))[
  study %in% c("GEDA", "KV"),
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_chd_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                    `prvl_rate_2.5%` = 0,
                    `prvl_rate_97.5%` = 0,
                    sex = ifelse(sex == "male", "men", "women"),
                    agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_chd_prev, ext_chd_prev)

prev_dat[, sex := ifelse(sex == "women", "Female", "Male")][, agegrp := paste(agegrp, "years")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") + 
               scale_fill_viridis_d(name = "Age group") +
               scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
               ggtitle("GEDA 2014") + 
               ylab("Prevalence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("GEDA 2019") + 
               ylab("Prevalence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


# Source: KV

# Year 2013-2018

ext3 <- ggplot(prev_dat[study == "KV" & year %in% c(2013:2018)]) +
  aes(x = year, y = `prvl_rate_50.0%`, color = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_line() + 
  scale_color_viridis_d(name = "Year") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  scale_x_continuous(breaks = seq(2013, 2018, 1)) +
  expand_limits(y = c(0, 0.45), x = c(2013, 2018)) +
  ggtitle("KV 2013-2018") + 
  ylab("Prevalence rate") +
  xlab("Year") +
  theme(#axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") + 
               geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                             position = dodge, width = 0.2) +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("IMPACT NCD Germany 2014") + 
               ylab("Prevalence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") +
               geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                             position = dodge, width = 0.2) +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("IMPACT NCD Germany 2019") + 
               ylab("Prevalence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2013-2018

imp3 <- ggplot(prev_dat[study == "IMPACT" & year %in% c(2013:2018)]) +
  aes(x = year, y = `prvl_rate_50.0%`, color = agegrp, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_line() +
  geom_ribbon(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`), alpha = 0.18, color = NA) +
  scale_color_viridis_d(name = "Age group") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  scale_x_continuous(breaks = seq(2013, 2018, 1)) +
  expand_limits(y = c(0, 0.45), x = c(2013, 2018)) +
  expand_limits(y = c(0, 0.45)) +
  ggtitle("IMPACT NCD Germany 2013-2018") + 
  ylab("Prevalence rate") +
  xlab("Year") +
  theme(axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_chd_prev_2014.tiff",
       height = 9, width = 16, dpi = 300)


plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_chd_prev_2019.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext3, imp3, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_chd_prev_2013_2019.tiff",
       height = 9, width = 16, dpi = 300)


## Stroke ----

# Prevalence

impact_stroke_prev <- d[
  scenario == "sc0" & disease == "stroke_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_stroke_prev <- fread(paste0(external_path, "stroke/ger_stroke_data_combined.csv"))[
  study %in% c("GEDA", "AOK"),
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_stroke_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                    `prvl_rate_2.5%` = 0,
                    `prvl_rate_97.5%` = 0,
                    sex = ifelse(sex == "male", "men", "women"),
                    agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_stroke_prev, ext_stroke_prev)

prev_dat[, sex := ifelse(sex == "women", "Female", "Male")][, agegrp := paste(agegrp, "years")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("GEDA 2014") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("GEDA 2019") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


# Source: AOK

# Year 2010

ext3 <- ggplot(prev_dat[study == "AOK" & year == 2010]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("AOK 2010") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2011

ext4 <- ggplot(prev_dat[study == "AOK" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("AOK 2011") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("IMPACT NCD Germany 2014") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("IMPACT NCD Germany 2019") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2013

imp3 <- ggplot(prev_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("IMPACT NCD Germany 2013") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_prev_2014.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_prev_2019.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext3, imp3, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_prev_2010.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext4, imp3, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_prev_2011.tiff",
       height = 9, width = 16, dpi = 300)


# Incidence

impact_stroke_incd <- e[
  scenario == "sc0" & disease == "stroke_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_stroke_incd <- fread(paste0(external_path, "stroke/ger_stroke_data_combined.csv"))[
  study %in% c("GEDA", "AOK"),
  c("year", "agegroup", "sex", "incidence", "comment", "study")
]

ext_stroke_incd[, `:=`(`prvl_rate_50.0%` = incidence/100,
                       `prvl_rate_2.5%` = 0,
                       `prvl_rate_97.5%` = 0,
                       sex = ifelse(sex == "male", "men", "women"),
                       agegrp = agegroup)][, c("incidence", "agegroup") := NULL]

incd_dat <- rbind(impact_stroke_incd, ext_stroke_incd)

incd_dat[, sex := ifelse(sex == "women", "Female", "Male")][, agegrp := paste(agegrp, "years")]

# Source: AOK

# Year 2010

ext1 <- ggplot(incd_dat[study == "AOK" & year == 2010]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("AOK 2010") + 
  ylab("Cumulative incidence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2011

ext2 <- ggplot(incd_dat[study == "AOK" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("AOK 2011") + 
  ylab("Cumulative incidence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2013

imp1 <- ggplot(incd_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("IMPACT NCD Germany 2013") + 
  ylab("Cumulative incidence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_incd_2010.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext2, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_stroke_incd_2011.tiff",
       height = 9, width = 16, dpi = 300)


## Type 2 Diabetes ----

# Prevalence

impact_t2dm_prev <- d[
  scenario == "sc0" & disease == "t2dm_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_t2dm_prev <- fread(paste0(external_path, "diabetes/ger_diabetes_data_combined.csv"))[
  study %in% c("GEDA", "Diab-Surv") & comment == "12-month",
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_t2dm_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                       `prvl_rate_2.5%` = 0,
                       `prvl_rate_97.5%` = 0,
                       sex = ifelse(sex == "male", "men", "women"),
                       agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_t2dm_prev, ext_t2dm_prev)

prev_dat[, sex := ifelse(sex == "women", "Female", "Male")][, agegrp := paste(agegrp, "years")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("GEDA 2014") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("GEDA 2019") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Source: Diabetes Surveillance

# Year 2011

ext3 <- ggplot(prev_dat[study == "Diab-Surv" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from the national surveillance 2011") + 
  ylab("12-month prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("IMPACT NCD Germany 2014") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("IMPACT NCD Germany 2019") + 
  ylab("Prevalence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Year 2013

imp3 <- ggplot(prev_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_t2dm_prev_2014.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_t2dm_prev_2019.tiff",
       height = 9, width = 16, dpi = 300)

plot_grid(ext3, imp3, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_t2dm_prev_2013.tiff",
       height = 9, width = 16, dpi = 300)


# Incidence

impact_t2dm_incd <- e[
  scenario == "sc0" & disease == "t2dm_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_t2dm_incd <- fread(paste0(external_path, "diabetes/ger_diabetes_data_combined.csv"))[
  study %in% c("GEDA", "Diab-Surv") & comment == "12-month",
  c("year", "agegroup", "sex", "incidence", "comment", "study")
]

ext_t2dm_incd[, `:=`(`prvl_rate_50.0%` = incidence/100,
                     `prvl_rate_2.5%` = 0,
                     `prvl_rate_97.5%` = 0,
                     sex = ifelse(sex == "male", "men", "women"),
                     agegrp = agegroup)][, c("incidence", "agegroup") := NULL]

incd_dat <- rbind(impact_t2dm_incd, ext_t2dm_incd)

incd_dat[, sex := ifelse(sex == "women", "Female", "Male")][, agegrp := paste(agegrp, "years")]

# Source: Diabetes Surveillance

# Year 2011

ext1 <- ggplot(incd_dat[study == "Diab-Surv" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Type 2 diabetes incidence in Germany from the national surveillance 2011") + 
  ylab("12-month incidence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2013

imp1 <- ggplot(incd_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Type 2 diabetes incidence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month incidence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(angle = 90))


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./validation_internal/external_sources/validation_ext_t2dm_incd_2011.tiff",
       height = 9, width = 12, dpi = 300)

















