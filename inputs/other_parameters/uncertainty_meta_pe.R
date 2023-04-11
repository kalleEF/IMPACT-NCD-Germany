library(data.table)
library(fst)

#### Simulate uncertainty meta-analytic price elasticity -------------------------------

meta_pe <- -6.74  # Data from Afshin paper
meta_pe_se <- (meta_pe + 10.40)/1.96

n_samples <- 10000

## Ensure replicability #  
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

meta_pe_samples <- data.table(
  mc = 1:n_samples,
  meta_pe = qnorm(quantiles, mean = meta_pe, sd = meta_pe_se)/10
)

write_fst(meta_pe_samples, "./inputs/other_parameters/meta_pe.fst")

