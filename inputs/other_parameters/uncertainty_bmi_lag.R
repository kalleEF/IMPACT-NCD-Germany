
library(data.table)
library(fst)

#### Simulate uncertainty in BMI lag -------------------------------

bmi_lag <- 3 # Data from Staudigel EVS paper/tables

n_samples <- 10000

bmi_lag_samples <- data.table(
  mc = 1:n_samples,
  bmi_lag = runif(n_samples, min = 1, max = 5)
)

write_fst(bmi_lag_samples, "./inputs/other_parameters/bmi_lag.fst")
