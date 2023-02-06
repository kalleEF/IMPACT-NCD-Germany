
library(data.table)
lc <- fread("./outputs/lifecourse/3_lifecourse.csv.gz")

cov <- matrix(c(0.3, 0.2, 0.1,
                -0.4, 0.001, 0.3,
                0.04, -0.01, 0.2), nrow = 3)

lc[, sex_num := ifelse(sex == "women", 1, 0)]

est_utility <- function(age, sex_num, bmi_curr_xps){
  
  utility_se <- sqrt(
    c(age, sex_num, bmi_curr_xps)
    
    %*%
      
      as.matrix(cov)
    
    %*%
      
      c(age, sex_num, bmi_curr_xps)
  )
  
  return(utility_se)
  
}

system.time(lc[, utility_se := mapply(est_utility, age, sex_num, bmi_curr_xps)])

system.time(
lc[, `:=`(x1_1 = 0.3, x1_2 = 0.2, x1_3 = 0.1,
          x2_1 = -0.4, x2_2 = 0.001, x2_3 = 0.3,
          x3_1 = 0.04, x3_2 = -0.01, x3_3 = 0.2)])

system.time(lc[, `:=`(tmp1 = age * x1_1 + sex_num * x2_1 + bmi_curr_xps * x3_1,
                      tmp2 = age * x1_2 + sex_num * x2_2 + bmi_curr_xps * x3_2,
                      tmp3 = age * x1_3 + sex_num * x2_3 + bmi_curr_xps * x3_3)])

system.time(lc[, utility_se_man := sqrt(tmp1 * age + tmp2 * sex_num + tmp3 * bmi_curr_xps)])



cov_long <- c(NULL)

for(i in 1:nrow(cov)){

  tmp <- t(cov[, i])
  
  colnames(tmp) <- paste0(seq(1, length(tmp), 1), "_", i)
  
cov_long <- cbind(cov_long, tmp)

}




