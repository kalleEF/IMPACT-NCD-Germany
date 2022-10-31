
sp <- qread("./simulation/tmp_s.qs")
sp2 <- qread("./simulation/tmp_s.qs")
setDT(sp$pop)
setDT(sp2$pop)

all.equal(sp$pop, sp2$pop)

# 
# source("./auxil/scenarios.R")
# scenario_fn <- scenario_1_fn
# scenario_fn(sp)
# 

all.equal(sp$pop, sp2$pop)

#diseases_b <- diseases

lapply(diseases, function(x) {
  print(x$name)
  x$gen_parf(sp, design)$
    set_init_prvl(sp, design)$
    set_rr(sp, design)$
    set_incd_prb(sp, design)$
    set_dgns_prb(sp, design)$
    set_mrtl_prb(sp, design)
})

lapply(diseases, function(x) {
  print(x$name)
  x$gen_parf(sp2, design)$
    set_init_prvl(sp2, design)$
    set_rr(sp2, design)$
    set_incd_prb(sp2, design)$
    set_dgns_prb(sp2, design)$
    set_mrtl_prb(sp2, design)
})

all.equal(sp$pop, sp2$pop)
l <- mk_scenario_init2("", diseases, sp, design)
l2 <- mk_scenario_init2("", diseases, sp2, design)
all.equal(l, l2)
simcpp(sp$pop, l, sp$mc_aggr)
simcpp(sp2$pop, l2, sp2$mc_aggr)
all.equal(sp$pop, sp2$pop)

sp_NA <- sp$pop[is.na(all_cause_mrtl)]

sp_test <- copy(sp$pop)
sp_test[, `:=`(sex = NULL, pid_mrk = NULL)]

sp2_test <- copy(sp2$pop)
sp2_test[, `:=`(sex = NULL, pid_mrk = NULL)]

diff <- as.matrix(sp_test[year == 13]) - as.matrix(sp2_test[year == 13])
diff <- as.data.table(diff)
summary(diff)
