
library(data.table)
lc <- fread("./outputs/lifecourse/3_lifecourse.csv.gz")

# 
# cov <- matrix(c(0.3, 0.2, 0.1,
#                 -0.4, 0.001, 0.3,
#                 0.04, -0.01, 0.2), nrow = 3)

cov <- fst::read_fst("./inputs/other_parameters/health_util_covariance.fst")
est <- fst::read_fst("./inputs/other_parameters/health_util_estimates.fst", as.data.table = TRUE)

cov_long <- (NULL)

for(i in 1:nrow(cov)){
  
  tmp <- t(cov[, i])
  
  colnames(tmp) <- paste0("cov", seq(1, length(tmp), 1), "_", i)
  
  cov_long <- cbind(cov_long, tmp)
  
}

lc[, `:=`(Intercept = 1,
          sex_num = ifelse(sex == "men", 0, 1),
          copd = 0,
          cancer = 0,
          asthma = 0,
          bronchitis = 0,
          t2dm = ifelse(t2dm_prvl > 0, 1, 0),
          hypertension = 0,
          t2dm_hypt = 0,
          stroke = ifelse(stroke_prvl > 0, 1, 0),
          t2dm_stroke = ifelse(t2dm_prvl > 0 & stroke_prvl > 0, 1, 0),
          arrhythmia = 0,
          t2dm_arythm = 0,
          chd = ifelse(chd_prvl > 0, 1, 0),
          t2dm_chd = ifelse(t2dm_prvl > 0 & chd_prvl > 0, 1, 0))]

j <- 0

for(i in seq(1, ncol(cov_long), 17)){
  
  j <- j + 1
  
  cols <- seq(i, i + 16, 1)
  
  row <- seq(1, length(cols), 1)
  
  lc[, (colnames(cov_long)[cols]) := as.data.table(t(cov_long[cols]))]
  
  lc[, (paste0("tmp", i)) := Intercept * get(paste0("cov", row[1], "_", j)) + age * get(paste0("cov", row[2], "_", j)) +
                             sex_num * get(paste0("cov", row[3], "_", j)) + bmi_curr_xps * get(paste0("cov", row[4], "_", j)) +
                             copd * get(paste0("cov", row[5], "_", j)) + cancer * get(paste0("cov", row[6], "_", j)) +
                             asthma * get(paste0("cov", row[7], "_", j)) + bronchitis * get(paste0("cov", row[8], "_", j)) +
                             t2dm * get(paste0("cov", row[9], "_", j)) + hypertension * get(paste0("cov", row[10], "_", j)) +
                             t2dm_hypt * get(paste0("cov", row[11], "_", j)) + stroke * get(paste0("cov", row[12], "_", j)) +
                             t2dm_stroke * get(paste0("cov", row[13], "_", j)) + arrhythmia * get(paste0("cov", row[14], "_", j)) +
                             t2dm_arythm * get(paste0("cov", row[15], "_", j)) + chd * get(paste0("cov", row[16], "_", j)) +
                             t2dm_chd * get(paste0("cov", row[17], "_", j))]
  
  lc[, (grep("cov", names(lc))) := NULL]

}

lc[, utility_se_man := sqrt(Intercept * tmp1 + age * tmp18 + sex_num * tmp35 +
                            bmi_curr_xps * tmp52 + copd * tmp69 + cancer * tmp86 +
                            asthma * tmp103 + bronchitis * tmp120 + t2dm * tmp137 +
                            hypertension * tmp154 + t2dm_hypt * tmp171 +
                            stroke * tmp188 + t2dm_stroke * tmp205 + arrhythmia * tmp222 +
                            t2dm_arythm * tmp239 + chd * tmp256 + t2dm_chd * tmp273)][, (grep("tmp", names(lc))) := NULL]






cov <- fst::read_fst("./inputs/other_parameters/health_util_covariance.fst", as.data.table = TRUE)
est <- fst::read_fst("./inputs/other_parameters/health_util_estimates.fst", as.data.table = TRUE)



est_utility <- function(Intercept, age, sex_num, bmi_curr_xps,
                        copd, cancer, asthma, bronchitis, t2dm, hypertension, t2dm_hypt,
                        stroke, t2dm_stroke, arrhythmia, t2dm_arythm, chd, t2dm_chd){
  
  utility_se <- sqrt(
    c(Intercept, age, sex_num, bmi_curr_xps,
      copd, cancer, asthma, bronchitis, t2dm, hypertension, t2dm_hypt,
      stroke, t2dm_stroke, arrhythmia, t2dm_arythm, chd, t2dm_chd)
    
    %*%
      
      as.matrix(cov)
    
    %*%
      
      c(Intercept, age, sex_num, bmi_curr_xps,
        copd, cancer, asthma, bronchitis, t2dm, hypertension, t2dm_hypt,
        stroke, t2dm_stroke, arrhythmia, t2dm_arythm, chd, t2dm_chd)
  )
  
  return(utility_se)
  
}

lc[, utility_se := mapply(est_utility, Intercept, age, sex_num, bmi_curr_xps,
                          copd, cancer, asthma, bronchitis, t2dm, hypertension, t2dm_hypt,
                          stroke, t2dm_stroke, arrhythmia, t2dm_arythm, chd, t2dm_chd)]




