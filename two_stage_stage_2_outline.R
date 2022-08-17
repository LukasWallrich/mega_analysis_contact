# Partly derived from code by Lena Keller - acommpanying manuscript by Brunner et al. (2022)

#---- Load packages

library(metafor)
library(clubSandwich)
library(tidyverse)

#--------------------------------------------------------------
# Three-Level Random-Effects Model: M___
#--------------------------------------------------------------

ES_M1 <- read_csv("_____")


# Three-level random-effects model to estimate tau2 at level 2 and level 3
res_beta_gender <- rma.mv(yi = yi, V = se_i^2, random = ~ 1 | cnt/cycle,
                       data = ES_M1, method = "REML", sparse = TRUE)
res_beta_gender

conf_int(res_beta_gender, vcov = "CR2")


# 95% prediction interval 
predict(res_beta_gender,  addx = TRUE)

# Profile likelihood method to check convergence of the model
# Maxima should be observed at the estimated values of tau2 at level 2 and level 3
# http://www.metafor-project.org/doku.php/analyses:konstantopoulos2011

profile_beta_gender_tau2_L3 <- profile(res_beta_gender, sigma2=1)
profile_beta_gender_tau2_L2 <-profile(res_beta_gender, sigma2=2)

# Profile Likelihood method to estimate 95% CI for tau2 at Level 2 and Level 3
CI_res_beta_gender <- confint(res_beta_gender, fixed = FALSE, random = TRUE)
CI_res_beta_gender


### Computation of I^2
# https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
W <- diag(1 / ES_M1$se_i^2)
X <- model.matrix(res_beta_gender)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W

#I2 total
100 * sum(res_beta_gender$sigma2) / (sum(res_beta_gender$sigma2) + (res_beta_gender$k-res_beta_gender$p)/sum(diag(P)))

#I2 for Level 3 / Level 2
100 * res_beta_gender$sigma2 / (sum(res_beta_gender$sigma2) + (res_beta_gender$k-res_beta_gender$p)/sum(diag(P)))



# Alternative parametrization of three-level random-effects model to estimate tau2_total / and SD_total of true effects

res_beta_gender_tot <- rma.mv(yi = yi, V = se_i^2, 
                             random = ~ factor(cycle) | cnt ,
                             data = ES_M1, method = "REML")
res_beta_gender_tot



# Profile Likelihood method to check convergence of the model
# maxima should be observed at the estimated values of tau2  and rho
# http://www.metafor-project.org/doku.php/analyses:konstantopoulos2011
profile(res_beta_gender_tot, tau2=1)
profile(res_beta_gender_tot, rho=1)



# 95 % prediction interval (the 95% PI is identical to the one obtained by using the three-level 
# random-effects model to estimate tau2 at Level 2 and 3)
predict(res_beta_gender_tot,  addx = TRUE)


# Profile Likelihood method to estimate 95% CI for tau2 total and rho
CI_tau_beta_gender_tot <- confint(res_beta_gender_tot, fixed = FALSE, random = TRUE)
CI_tau_beta_gender_tot