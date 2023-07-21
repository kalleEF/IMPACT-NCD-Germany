

lc <- fread("./outputs/Test/lifecourse/3_lifecourse.csv.gz")[year %in% c(23, 43)]

tbl <-
  read_fst("./inputs/exposure_distributions/juice_sugar_table.fst",
           as.data.table = TRUE)

tbl[, juice_sugar := qBCPE(0.5, mu, sigma, nu, tau)]

absorb_dt(lc, tbl[, c("sex", "age", "juice_sugar")])


tbl <-
  read_fst("./inputs/exposure_distributions/ssb_sugar_table.fst",
           as.data.table = TRUE)

tbl[, ssb_sugar := qBCPEo(0.5, mu, sigma, nu, tau)]

absorb_dt(lc, tbl[, c("sex", "age", "ssb_sugar")])

lc[, bev_sugar := juice_sugar * juice_curr_xps + ssb_sugar * ssb_curr_xps]

# Random sugar delta for testing
lc[, sugar_delta := ifelse(year == 23, 0, rnorm(.N, -2, 0.2))]


lc[, sugar_delta_perc := abs(sugar_delta)/shift(bev_sugar) * 100, by = "pid"]


lc[year == 43, lapply(.SD, weighted.mean, wt, na.rm = TRUE), .SDcols = "sugar_delta_perc", keyby = c("scenario", "year", "agegrp", "sex")]




tbl <-
  read_fst("./inputs/exposure_distributions/juice_sugar_table.fst",
           as.data.table = TRUE)

tbl[, juice_sugar := qBCPE(0.5, mu, sigma, nu, tau)]

juice_sugar <- mean(tbl[, juice_sugar])

tbl <-
  read_fst("./inputs/exposure_distributions/ssb_sugar_table.fst",
           as.data.table = TRUE)

tbl[, ssb_sugar := qBCPEo(0.5, mu, sigma, nu, tau)]

ssb_sugar <- mean(tbl[, ssb_sugar])


tt[, `:=`(juice_sugar = juice_sugar, ssb_sugar = ssb_sugar)]
tt[, `:=`(bev_sugar = juice_curr_xps * juice_sugar + ssb_curr_xps * ssb_sugar)]

tt <- copy(tt[year %in% c(23,43)])

####SOLUTION####

# Recomupte original sugar consumption at baseline (2023) using median sugar per bev (see above)
# This works for all scenarios because baseline consumption is accurate
# Actual sugar consumption in 2043 is not recoverable because of the direct SSB effect workaround in SC3
# This solution uses the actual computed sugar reduction in 2043 and the recomputed sugar consumption at baseline.

tt[, sugar_delta_perc := abs(sugar_delta_xps)/shift(sugar_test) * 100, by = .(mc, agegrp, sex, scenario)]


