

#### Process uncertainty outputs ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
theme_set(new = theme_economist())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))


## Prevalence ## ----

tt <- fread("./outputs/summaries/prvl_scaled_up.csv.gz"
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, "./outputs/tables/prevalence_rate_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/prevalence_rate_by_year.tiff",
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/prevalence_rate_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/prevalence_rate_diff_by_year.tiff",
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, "./outputs/tables/prevalence_numbers_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/prevalence_numbers_by_year.tiff",
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/prevalence_numbers_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/prevalence_numbers_diff_by_year.tiff",
       height = 9, width = 16)

# Case-years prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "sex"]
  setnames(dd, c("sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/case_years_prev_post_by_scenario.csv")

## Incidence ## ----

tt <- fread("./outputs/summaries/incd_scaled_up.csv.gz"
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, "./outputs/tables/incidence_rate_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/incidence_rate_by_year.tiff",
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/incidence_rate_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/incidence_rate_diff_by_year.tiff",
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, "./outputs/tables/incidence_numbers_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/incidence_numbers_by_year.tiff",
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/incidence_numbers_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/incidence_numbers_diff_by_year.tiff",
       height = 9, width = 16)

# Cases prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "sex"]
  setnames(dd, c("sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/cases_prev_post_by_scenario.csv")

## Disease-specific Mortality ## ----

# WARNING: For some reason some iterations have a trailing comma!
file_lines <- readLines("./outputs/summaries/dis_mrtl_scaled_up.csv.gz")
writeLines(gsub(",+$", "", file_lines), "./outputs/summaries/dis_mrtl_scaled_up.csv.gz")

tt <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz", fill = TRUE,
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "agegrp", "sex", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, "./outputs/tables/disease_mortality_rate_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/disease_mortality_rate_by_year.tiff",
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/disease_mortality_rate_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/disease_mortality_rate_diff_by_year.tiff",
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, "./outputs/tables/disease_mortality_numbers_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/disease_mortality_numbers_by_year.tiff",
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/disease_mortality_numbers_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/disease_mortality_numbers_diff_by_year.tiff",
       height = 9, width = 16)


## All-cause Mortality ## ----

tt <- fread("./outputs/summaries/mrtl_scaled_up.csv.gz"
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "agegrp", "sex", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, "./outputs/tables/mortality_rate_by_year.csv")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/mortality_rate_by_year.tiff",
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("all_cause_mrtl")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-2]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-2])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/mortality_rate_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/mortality_rate_diff_by_year.tiff",
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, "./outputs/tables/mortality_numbers_by_year.csv")

ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/mortality_numbers_by_year.tiff",
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

prvls <- c("all_cause_mrtl")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-2]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-2])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, "./outputs/tables/mortality_numbers_diff_by_year.csv")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave("./outputs/plots/mortality_numbers_diff_by_year.tiff",
       height = 9, width = 16)

## Cost-effectiveness ## ----

cea <- fread("./outputs/summaries/cea_results.csv.gz")

cea_agg <- cea[, lapply(.SD, sum), .SDcols = !c("scenario", "sex", "agegrp_start", "mc"),
               by = c("scenario", "mc")]

ggplot(cea_agg[scenario != "sc0"], aes(x = incr_qaly_scl,
                    y = incr_cost_scl,
                    col = scenario)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  expand_limits(x = -5e4, y = 1e9) +
  scale_y_continuous(name = "Incremental healthcare costs") +
  scale_x_continuous(name = "Incremental QALYs") +
  ggtitle("Cost-effectiveness plane")


ggsave("./outputs/plots/cost_effectiveness_plane.tiff",
       height = 9, width = 16)
























#### OLD CODE FROM CHRIS ####

# 
# 
# 
# 
# tt_esp <- fread("./outputs/summaries/prvl_scaled_up.csv.gz"
# )[, `:=` (year = year + 2000L)]
# 
# outstrata <- c("mc", "year", "scenario")
# e <- tt_esp[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
# ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
# e <- melt(e, id.vars = outstrata)
# e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(e, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_esp_")))
# fwrite(e, "/mnt/storage_fast/output/hf_real/tables/age-standardised prevalence by year and dimd.csv")
# 
# e[disease == "cmsmm0_prvl", disease := "CMS score > 0"]
# e[disease == "cmsmm1_prvl", disease := "CMS score > 1"]
# e[disease == "cmsmm1.5_prvl", disease := "CMS score > 1.5"]
# e[disease == "cmsmm2_prvl", disease := "CMS score > 2"]
# e <- e[between(year, 2014, 2040) & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")]
# e[scenario == "sc0", scenario := "Base-case"]
# e[scenario == "sc1", scenario := "10% BMI reduction"]
# 
# ggplot(e[scenario == "Base-case" & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~disease)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd.png", scale = 1.5, width = 16/2, height = 9/2)
# 
# e[year %in% c(2020, 2040) & dimd %in% c("1 most deprived", "10 least deprived") & disease == "CMS score > 1.5" & scenario == "Base-case"]
# 
# ggplot(e[disease %in% c("CMS score > 1.5")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised, CMS score > 1.5)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~ scenario)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd scenario.png", scale = 1.5, width = 16/2, height = 9/2)
# 
# e[year %in% c(2020, 2040) & dimd %in% c("1 most deprived", "10 least deprived") & disease == "CMS score > 1.5" & scenario == "10% BMI reduction"]
# 
# 
# fl <- list.files("/mnt/storage_fast/output/hf_real/lifecourse",
#                  "_lifecourse.csv.gz$", full.names = TRUE)
# 
# 
# out <- rbindlist(lapply(fl, fread))[scenario == "sc0", ]
# out[, dimd := factor(dimd, c("1 most deprived", as.character(2:9), "10 least deprived"))]
# setkey(out, mc, pid, year)
# out[, table(cmsmm1.5_prvl)]
# View(out[cmsmm1.5_prvl > 0, .(mc, pid, year, cmsmm1.5_prvl) ])
# t1 <- out[cmsmm1.5_prvl == 1 & year %in% c(20, 40), .(mm_free = weighted.mean(age, wt_esp)), keyby = .(year, sex)] # mean age of multimorbidity
# t2 <- out[all_cause_mrtl > 0 & year %in% c(20, 40), .(le = weighted.mean(age, wt_esp)), keyby = .(year, sex)] # mean life expectancy
# t1 <- t1[t2]
# 
# t1 <- out[cmsmm1.5_prvl == 1 & year %in% c(20, 40), .(mm_free = weighted.mean(age, wt_esp)), keyby = .(year, dimd)] # mean age of multimorbidity
# t2 <- out[all_cause_mrtl > 0 & year %in% c(20, 40), .(le = weighted.mean(age, wt_esp)), keyby = .(year, dimd)] # mean life expectancy
# t1 <- t1[t2]
# View(t1)
# 
# 
# tt_esp <- fread("/mnt/storage_fast/output/hf_real/summaries/incd_esp.csv.gz"
# )[, `:=` (year = year + 2000L,
#           dimd = factor(dimd, c("1 most deprived", as.character(2:9), "10 least deprived")))]
# outstrata <- c("mc", "year", "dimd", "scenario")
# e <- tt_esp[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
# ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
# e <- melt(e, id.vars = outstrata)
# e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(e, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_esp_")))
# # fwrite(e, "/mnt/storage_fast/output/hf_real/tables/age-standardised prevalence by year and dimd.csv")
# 
# e[disease == "cmsmm0_prvl", disease := "CMS score > 0"]
# e[disease == "cmsmm1_prvl", disease := "CMS score > 1"]
# e[disease == "cmsmm1.5_prvl", disease := "CMS score > 1.5"]
# e[disease == "cmsmm2_prvl", disease := "CMS score > 2"]
# e <- e[between(year, 2014, 2040) & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")]
# e[scenario == "sc0", scenario := "Base-case"]
# e[scenario == "sc1", scenario := "10% BMI reduction"]
# 
# ggplot(e[scenario == "Base-case" & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~disease)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd.png", scale = 1.5, width = 16/2, height = 9/2)
