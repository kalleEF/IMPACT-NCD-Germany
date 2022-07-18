res <- fread("./outputs/summaries/prvl_out.csv.gz")

ggplot(res[mc == 8], aes(year, t2dm_prvl/popsize, linetype = scenario, col = agegrp)) +
  facet_wrap(~ sex) +
  geom_line()


mrtl <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz")

ggplot(mrtl[mc == 1 & scenario == "sc1" & sex == "men"], aes(x = year, y = chd)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_line()
