
#### Estimation of input distributions for PRIMEtime-CE ####

library(data.table)
library(foreach)
library(Hmisc)
library(CKutils)
library(fst)

doParallel::registerDoParallel(32)

# Aggregate SynthPops # ----

synthpops <- grep(pattern = ".fst", list.files("/media/php-workstation/Storage_1/IMPACT_Storage/inputs/synthpop/"), value = TRUE)

ids <- sample(1:length(synthpops), 300, replace = FALSE)

tt <- read_fst("./inputs/pop_projections/German_pop_combined.fst", as.data.table = TRUE)

dat <- foreach(i = seq(synthpops)[ids],
               .packages = c("fst", "data.table")) %dopar% {
                 
                 xx <- read_fst(paste0("/media/php-workstation/Storage_1/IMPACT_Storage/inputs/synthpop/", synthpops[i]), as.data.table = TRUE)[
                   year >= 13, .(year, age, sex,
                                 bmi_curr_xps, ssb_curr_xps, juice_curr_xps,
                                 sugar_per_ssb, sugar_per_juice)]
                 
                 tt <- tt[between(age, min(xx$age), max(xx$age)) &
                            between(year, min(xx$year), max(xx$year)),
                          .(pops = sum(pops)), keyby = .(year, age, sex)]
                 xx[, wt_immrtl := .N, by = .(year, age, sex)]
                 absorb_dt(xx, tt)
                 xx[, wt := pops / (wt_immrtl * 1)]
                 xx[, pops := NULL]
                 
                 return(xx)
                 
               }

dat_bind <- rbindlist(dat)

rm(dat)


# BMI distribution #

dat_mean <- dat_bind[, lapply(.SD, wtd.mean, weights = wt), # weighted mean
                     .SDcols = "bmi_curr_xps",
                     by = .(sex, age, year)]
setnames(dat_mean, "bmi_curr_xps", "BMI_mean")

dat_sd <- dat_bind[, lapply(.SD, wtd.var, weights = wt), # weighted variance
                   .SDcols = "bmi_curr_xps",
                   by = .(sex, age, year)]
setnames(dat_sd, "bmi_curr_xps", "BMI_sd")
dat_sd[, BMI_sd := sqrt(BMI_sd)]

bmi_synthpop_table <- merge(dat_mean, dat_sd, by = c("sex", "age", "year"))

ggplot(bmi_synthpop_table, aes(year, BMI_mean, col = factor(age))) +
  facet_wrap(~ sex) +
  geom_line()

bmi_synthpop_table[, sex := ifelse(sex == "men", "male", "female")]

setkey(bmi_synthpop_table, sex, year)

fwrite(bmi_synthpop_table, "/media/php-workstation/Storage_1/IMPACT_Storage/PRIMEtime_export/bmi_synthpop_input_2013_2035.csv", sep = ";", dec = ".")



## SSB distribution ##

dat_mean <- dat_bind[, lapply(.SD, wtd.mean, weights = wt),
                     .SDcols = "ssb_curr_xps",
                     by = .(sex, age)]
setnames(dat_mean, "ssb_curr_xps", "SSB_mean")

dat_sd <- dat_bind[, lapply(.SD, wtd.var, weights = wt),
                   .SDcols = "ssb_curr_xps",
                   by = .(sex, age)]
setnames(dat_sd, "ssb_curr_xps", "SSB_sd")
dat_sd[, SSB_sd := sqrt(SSB_sd)]

ssb_synthpop_table <- merge(dat_mean, dat_sd, by = c("sex", "age"))

ggplot(ssb_synthpop_table, aes(age, SSB_mean)) +
  facet_wrap(~ sex) +
  geom_line()

ssb_synthpop_table[, sex := ifelse(sex == "men", "male", "female")]

setkey(ssb_synthpop_table, sex)

fwrite(ssb_synthpop_table, "/media/php-workstation/Storage_1/IMPACT_Storage/PRIMEtime_export/ssb_synthpop_input_2013.csv", sep = ";", dec = ".")



## Fruit juice distribution ##

dat_mean <- dat_bind[, lapply(.SD, wtd.mean, weights = wt),
                     .SDcols = "juice_curr_xps",
                     by = .(sex, age)]
setnames(dat_mean, "juice_curr_xps", "Juice_mean")

dat_sd <- dat_bind[, lapply(.SD, wtd.var, weights = wt),
                   .SDcols = "juice_curr_xps",
                   by = .(sex, age)]
setnames(dat_sd, "juice_curr_xps", "Juice_sd")
dat_sd[, Juice_sd := sqrt(Juice_sd)]

juice_synthpop_table <- merge(dat_mean, dat_sd, by = c("sex", "age"))

ggplot(juice_synthpop_table, aes(age, Juice_mean)) +
  facet_wrap(~ sex) +
  geom_line()

juice_synthpop_table[, sex := ifelse(sex == "men", "male", "female")]

setkey(juice_synthpop_table, sex)

fwrite(juice_synthpop_table, "/media/php-workstation/Storage_1/IMPACT_Storage/PRIMEtime_export/juice_synthpop_input_2013.csv", sep = ";", dec = ".")


## SSB sugar table ## ----

dat_mean <- dat_bind[, lapply(.SD, wtd.mean, weights = wt), # weighted_mean
                     .SDcols = "sugar_per_ssb",
                     by = .(sex, age)]
setnames(dat_mean, "sugar_per_ssb", "SSB_sugar_mean")

dat_sd <- dat_bind[, lapply(.SD, wtd.var, weights = wt), # weighted_sd
                   .SDcols = "sugar_per_ssb",
                   by = .(sex, age)]
setnames(dat_sd, "sugar_per_ssb", "SSB_sugar_sd")
dat_sd[, SSB_sugar_sd := sqrt(SSB_sugar_sd)]

ssb_sugar_table <- merge(dat_mean, dat_sd, by = c("sex", "age"))

ggplot(ssb_sugar_table, aes(age, SSB_sugar_mean)) +
  facet_wrap(~ sex) +
  geom_line()

ssb_sugar_table[, sex := ifelse(sex == "men", "male", "female")]

setkey(ssb_sugar_table, sex)

fwrite(ssb_sugar_table, "/media/php-workstation/Storage_1/IMPACT_Storage/PRIMEtime_export/ssb_sugar_input_2013.csv", sep = ";", dec = ".")


## Juice sugar table ## ----

dat_mean <- dat_bind[, lapply(.SD, wtd.mean, weights = wt), # weighted_mean
                     .SDcols = "sugar_per_juice",
                     by = .(sex, age)]
setnames(dat_mean, "sugar_per_juice", "Juice_sugar_mean")

dat_sd <- dat_bind[, lapply(.SD, wtd.var, weights = wt), # weighted_sd
                   .SDcols = "sugar_per_juice",
                   by = .(sex, age)]
setnames(dat_sd, "sugar_per_juice", "Juice_sugar_sd")
dat_sd[, Juice_sugar_sd := sqrt(Juice_sugar_sd)]

juice_sugar_table <- merge(dat_mean, dat_sd, by = c("sex", "age"))

ggplot(juice_sugar_table, aes(age, Juice_sugar_mean)) +
  facet_wrap(~ sex) +
  geom_line()

juice_synthpop_table[, sex := ifelse(sex == "men", "male", "female")]

setkey(juice_sugar_table, sex)

fwrite(juice_sugar_table, "/media/php-workstation/Storage_1/IMPACT_Storage/PRIMEtime_export/juice_sugar_input_2013.csv", sep = ";", dec = ".")



