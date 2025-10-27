# required R packages:
# dplyr, stringr, demography, CADFtest, ggplot2, latex2exp, snowfall, rlecuyer

setwd('C:/Users/taowu/Desktop/Learning Files/mortality/单位根检验/美国')

## DATA IMPORT

# USA data, 5 x 1, male, female and total data
# http://www.mortality.org/cgi-bin/hmd/country.php?cntr=USA&level=1

# load raw data files
# to download data other than 1x1, do not use the 'demography' package functions

lifetbl_both <- read.table('America.txt', skip = 2, header = TRUE,
                           stringsAsFactors = FALSE)
lifetbl_male <- lifetbl_both[, -which(names(lifetbl_both) == "Female")]
lifetbl_male <- lifetbl_male[, -which(names(lifetbl_male) == "Total")]


# lifetbl_male <- read.table('mltper_5x1.txt', skip = 2, header = TRUE,stringsAsFactors = FALSE)


lifetbl_female <- lifetbl_both[, -which(names(lifetbl_both) == "Male")]
lifetbl_female <- lifetbl_female[, -which(names(lifetbl_female) == "Total")]


# lifetbl_female <- read.table('fltper_5x1.txt', skip = 2, header = TRUE,stringsAsFactors = FALSE)



## DATA CLEANUP

# replace 110+ with 110, change data format to numeric
library(dplyr)
library(stringr)

lifetbl_both <- mutate(lifetbl_both, Age0 = as.numeric(str_extract(lifetbl_both$Age,
                                                                   '[0-9]+')))
lifetbl_male <- mutate(lifetbl_male, Age0 = as.numeric(str_extract(lifetbl_male$Age,
                                                                   '[0-9]+')))
lifetbl_female <- mutate(lifetbl_female, Age0 =
                           as.numeric(str_extract(lifetbl_female$Age, '[0-9]+')))

# focus on ages of 25 - 74
lifetbl_both <- lifetbl_both[lifetbl_both$Age0 >= 25 & lifetbl_both$Age0 < 75, ]
lifetbl_male <- lifetbl_male[lifetbl_male$Age0 >= 25 & lifetbl_male$Age0 < 75, ]
lifetbl_female <- lifetbl_female[lifetbl_female$Age0 >= 25 & lifetbl_female$Age0 < 75, ]

lifetbl_both <- lifetbl_both[#lifetbl_both$Year >=1919 & 
  lifetbl_both$Year < 2020, ]
lifetbl_male <- lifetbl_male[#lifetbl_male$Year >= 1919 & 
  lifetbl_male$Year < 2020, ]
lifetbl_female <- lifetbl_female[#lifetbl_female$Year>= 1919 & 
  lifetbl_female$Year < 2020, ]

# n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both$Age0)) # n_agegroups = 10

# n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both$Year)) # n_periods = 66

# create mx
lifetbl_both$mx <- as.numeric(lifetbl_both$Total)    
lifetbl_male$mx <- as.numeric(lifetbl_male$Male)  
lifetbl_female$mx <- as.numeric(lifetbl_female$Female)     




## IMPLEMENTATION OF THE LEE-CARTER MODEL

library(demography)

# wrap up available data for input into 'lca' function
LC_wrapped_both <- demogdata(matrix(data = lifetbl_both$mx, nrow = n_agegroups,
                                    ncol = n_periods, byrow = FALSE), pop = matrix(0, n_agegroups, n_periods),
                             ages = unique(lifetbl_both$Age0), years = unique(lifetbl_both$Year),
                             type = 'mortality', label = 'UK_5x1', name = 'total', lambda = 0)

LC_wrapped_male <- demogdata(matrix(data = lifetbl_male$mx, nrow = n_agegroups,
                                    ncol = n_periods, byrow = FALSE), pop = matrix(0, n_agegroups, n_periods),
                             ages = unique(lifetbl_both$Age0), years = unique(lifetbl_both$Year),
                             type = 'mortality', label = 'UK_5x1', name = 'male', lambda = 0)

LC_wrapped_female <- demogdata(matrix(data = lifetbl_female$mx, nrow = n_agegroups,
                                      ncol = n_periods, byrow = FALSE), pop = matrix(0, n_agegroups, n_periods),
                               ages = unique(lifetbl_both$Age0), years = unique(lifetbl_both$Year),
                               type = 'mortality', label = 'UK_5x1', name = 'female', lambda = 0)

# apply 'lca' function in 'demography' package
LC_LCA.fitting_both <- lca(LC_wrapped_both, adjust = 'none')
LC_LCA.fitting_male <- lca(LC_wrapped_male, adjust = 'none')
LC_LCA.fitting_female <- lca(LC_wrapped_female, adjust = 'none')

# ax
LC_ax_both <- LC_LCA.fitting_both$ax
LC_ax_male <- LC_LCA.fitting_male$ax
LC_ax_female <- LC_LCA.fitting_female$ax
names(LC_ax_both) <- names(LC_ax_male) <- names(LC_ax_female) <- NULL

# bx
LC_bx_both <- LC_LCA.fitting_both$bx
LC_bx_male <- LC_LCA.fitting_male$bx
LC_bx_female <- LC_LCA.fitting_female$bx
names(LC_bx_both) <- names(LC_bx_male) <- names(LC_bx_female) <- NULL

# kt
LC_kt_both <- array(LC_LCA.fitting_both$kt)
LC_kt_male <- array(LC_LCA.fitting_male$kt)
LC_kt_female <- array(LC_LCA.fitting_female$kt)

# below: fitting k_t with (5) in Draft #4
LC_kt1_both <- LC_kt_both[-1]
LC_kt1_male <- LC_kt_male[-1]
LC_kt1_female <- LC_kt_female[-1]

LC_kt0_both <- LC_kt_both[-n_periods]
LC_kt0_male <- LC_kt_male[-n_periods]
LC_kt0_female <- LC_kt_female[-n_periods]

LC_kt.fitting_both <- lm(LC_kt1_both ~ LC_kt0_both)
LC_kt.fitting_male <- lm(LC_kt1_male ~ LC_kt0_male)
LC_kt.fitting_female <- lm(LC_kt1_female ~ LC_kt0_female)

# report format: (estimate, standard error)
LC_mu_both <- LC_kt.fitting_both$coefficients[1]
LC_mu_male <- LC_kt.fitting_male$coefficients[1]
LC_mu_female <- LC_kt.fitting_female$coefficients[1]
names(LC_mu_both) <- names(LC_mu_male) <- names(LC_mu_female) <- NULL

LC_mu_se_both <- coef(summary(LC_kt.fitting_both))[, 'Std. Error'][1]
LC_mu_se_male <- coef(summary(LC_kt.fitting_male))[, 'Std. Error'][1]
LC_mu_se_female <- coef(summary(LC_kt.fitting_female))[, 'Std. Error'][1]
names(LC_mu_se_both) <- names(LC_mu_se_male) <- names(LC_mu_se_female) <- NULL

LC_phi_both <- LC_kt.fitting_both$coefficients[2]
LC_phi_male <- LC_kt.fitting_male$coefficients[2]
LC_phi_female <- LC_kt.fitting_female$coefficients[2]
names(LC_phi_both) <- names(LC_phi_male) <- names(LC_phi_female) <- NULL

LC_phi_se_both <- coef(summary(LC_kt.fitting_both))[, 'Std. Error'][2]
LC_phi_se_male <- coef(summary(LC_kt.fitting_male))[, 'Std. Error'][2]
LC_phi_se_female <- coef(summary(LC_kt.fitting_female))[, 'Std. Error'][2]
names(LC_phi_se_both) <- names(LC_phi_se_male) <- names(LC_phi_se_female) <- NULL

# Augmented Dickey-Fuller test for unit root
library(CADFtest)

LC_kt.test_both <- CADFtest(LC_kt_both ~ 1, type = 'drift', lags = 1)
LC_kt.test_male <- CADFtest(LC_kt_male ~ 1, type = 'drift', lags = 1)
LC_kt.test_female <- CADFtest(LC_kt_female ~ 1, type = 'drift', lags = 1)

LC_kt.test.p.value_both <- LC_kt.test_both$p.value
LC_kt.test.p.value_male <- LC_kt.test_male$p.value
LC_kt.test.p.value_female <- LC_kt.test_female$p.value

## IMPLEMENTATION OF THE ORIGINALLY PROPOSED STATISTICAL INFERENCE METHOD

# generate table of central mortality rate
OM_logM_both <- matrix(data = lifetbl_both$mx, nrow = n_agegroups, ncol = n_periods,
                       byrow = FALSE)
OM_logM_male <- matrix(data = lifetbl_male$mx, nrow = n_agegroups, ncol = n_periods,
                       byrow = FALSE)
OM_logM_female <- matrix(data = lifetbl_female$mx, nrow = n_agegroups, ncol = n_periods,
                         byrow = FALSE)

# dimension of mortality rate data:
# number of years observed (T, n_periods) X number of age groups (M, n_agegroups)
OM_logM_both <- t(log(OM_logM_both))
OM_logM_male <- t(log(OM_logM_male))
OM_logM_female <- t(log(OM_logM_female))

# compute \hat{Z}_t
OM_Zhat_t_both <- apply(OM_logM_both, 1, sum)
OM_Zhat_t_male <- apply(OM_logM_male, 1, sum)
OM_Zhat_t_female <- apply(OM_logM_female, 1, sum)

OM_Zhat_t1_both <- OM_Zhat_t_both[-1]
OM_Zhat_t1_male <- OM_Zhat_t_male[-1]
OM_Zhat_t1_female <- OM_Zhat_t_female[-1]

OM_Zhat_t0_both <- OM_Zhat_t_both[-n_periods]
OM_Zhat_t0_male <- OM_Zhat_t_male[-n_periods]
OM_Zhat_t0_female <- OM_Zhat_t_female[-n_periods]

OM_Zhat_t.fitting_both <- lm(OM_Zhat_t1_both ~ OM_Zhat_t0_both)
OM_Zhat_t.fitting_male <- lm(OM_Zhat_t1_male ~ OM_Zhat_t0_male)
OM_Zhat_t.fitting_female <- lm(OM_Zhat_t1_female ~ OM_Zhat_t0_female)

# report format: (estimate, standard error)
OM_mu_both <- OM_Zhat_t.fitting_both$coefficients[1]
OM_mu_male <- OM_Zhat_t.fitting_male$coefficients[1]
OM_mu_female <- OM_Zhat_t.fitting_female$coefficients[1]
names(OM_mu_both) <- names(OM_mu_male) <- names(OM_mu_female) <- NULL

OM_mu_se_both <- coef(summary(OM_Zhat_t.fitting_both))[, 'Std. Error'][1]
OM_mu_se_male <- coef(summary(OM_Zhat_t.fitting_male))[, 'Std. Error'][1]
OM_mu_se_female <- coef(summary(OM_Zhat_t.fitting_female))[, 'Std. Error'][1]
names(OM_mu_se_both) <- names(OM_mu_se_male) <- names(OM_mu_se_female) <- NULL

OM_phi_both <- OM_Zhat_t.fitting_both$coefficients[2]
OM_phi_male <- OM_Zhat_t.fitting_male$coefficients[2]
OM_phi_female <- OM_Zhat_t.fitting_female$coefficients[2]
names(OM_phi_both) <- names(OM_phi_male) <- names(OM_phi_female) <- NULL

OM_phi_se_both <- coef(summary(OM_Zhat_t.fitting_both))[, 'Std. Error'][2]
OM_phi_se_male <- coef(summary(OM_Zhat_t.fitting_male))[, 'Std. Error'][2]
OM_phi_se_female <- coef(summary(OM_Zhat_t.fitting_female))[, 'Std. Error'][2]
names(OM_phi_se_both) <- names(OM_phi_se_male) <- names(OM_phi_se_female) <- NULL

# define function: OM_estimate.alphabeta.x - estimate an element of \alpha_x and \beta_x
OM_estimate.alphabeta.x <- function(arg1, arg2) {
  # arg1: a column of OM_logM_XXXX
  # arg2: array, OM_Zhat_t_XXXX
  argsize <- length(arg1) # dimension of function input parameter
  # the two input arguments are assumed to have the same length, or
  # error will be reported
  
  # compute estimates
  a.x <- (sum(arg1) * sum(arg2 ^ 2) - sum(arg1 * arg2) * sum(arg2)) /
    (argsize * sum(arg2 ^ 2) - (sum(arg2)) ^ 2)
  b.x <- (argsize * sum(arg1 * arg2) - sum(arg1) * sum(arg2)) /
    (argsize * sum(arg2 ^ 2) - (sum(arg2)) ^ 2)
  
  return(c(a.x, b.x)) # return a pair of estimates of \alpha_x and \beta_x
} # end function OM_estimate.alphabeta.x

# \alpha_x and \beta_x
OM_axbx_both <- apply(OM_logM_both, 2, OM_estimate.alphabeta.x, OM_Zhat_t_both)
OM_axbx_male <- apply(OM_logM_male, 2, OM_estimate.alphabeta.x, OM_Zhat_t_male)
OM_axbx_female <- apply(OM_logM_female, 2, OM_estimate.alphabeta.x, OM_Zhat_t_female)

# choose the value of L (L = 5, 10, 15) for the Chi-square test
# (during implementation of our method)
OM_test.L <- floor(sqrt(n_periods))

# adjust \mu values for the test
OM_mu.adjusted_both <- OM_mu_both - OM_Zhat_t_both[1] + OM_phi_both * OM_Zhat_t_both[1]
OM_mu.adjusted_male <- OM_mu_male - OM_Zhat_t_male[1] + OM_phi_male * OM_Zhat_t_male[1]
OM_mu.adjusted_female <- OM_mu_female - OM_Zhat_t_female[1] +
  OM_phi_female * OM_Zhat_t_female[1]

# compute \hat{e}_t (t = 2 ... T)
OM_ehat_t_both <- OM_Zhat_t1_both - OM_mu_both - OM_phi_both * OM_Zhat_t0_both
OM_ehat_t_male <- OM_Zhat_t1_male - OM_mu_male - OM_phi_male * OM_Zhat_t0_male
OM_ehat_t_female <- OM_Zhat_t1_female - OM_mu_female - OM_phi_female * OM_Zhat_t0_female

OM_ehat_t1_both <- OM_ehat_t_both[-1]
OM_ehat_t1_male <- OM_ehat_t_male[-1]
OM_ehat_t1_female <- OM_ehat_t_female[-1]

OM_ehat_t0_both <- OM_ehat_t_both[-(n_periods - 1)]
OM_ehat_t0_male <- OM_ehat_t_male[-(n_periods - 1)]
OM_ehat_t0_female <- OM_ehat_t_female[-(n_periods - 1)]

# compute \hat{U}_i (i = 2 ... T-L+1)
OM_Uhat_i_both <- apply(embed(OM_ehat_t_both, OM_test.L), 1, sum) / OM_test.L
OM_Uhat_i_male <- apply(embed(OM_ehat_t_male, OM_test.L), 1, sum) / OM_test.L
OM_Uhat_i_female <- apply(embed(OM_ehat_t_female, OM_test.L), 1, sum) / OM_test.L

# \hat{U}_i (i = 1 ... T-L+1)
OM_Uhat_i_both <- c(sum(OM_ehat_t_both[2:OM_test.L]) / OM_test.L, OM_Uhat_i_both)
OM_Uhat_i_male <- c(sum(OM_ehat_t_male[2:OM_test.L]) / OM_test.L, OM_Uhat_i_male)
OM_Uhat_i_female <- c(sum(OM_ehat_t_female[2:OM_test.L]) / OM_test.L, OM_Uhat_i_female)

# compute \hat{\sigma}_e^2
OM_sigma.hat_e.sq_both <- OM_test.L * ((sum(OM_Uhat_i_both ^ 2)) / (n_periods -
                                                                      OM_test.L + 1) - ((sum(OM_Uhat_i_both)) / (n_periods - OM_test.L + 1)) ^ 2)
OM_sigma.hat_e.sq_male <- OM_test.L * ((sum(OM_Uhat_i_male ^ 2)) / (n_periods -
                                                                      OM_test.L + 1) - ((sum(OM_Uhat_i_male)) / (n_periods - OM_test.L + 1)) ^ 2)
OM_sigma.hat_e.sq_female <- OM_test.L * ((sum(OM_Uhat_i_female ^ 2)) / (n_periods -
                                                                          OM_test.L + 1) - ((sum(OM_Uhat_i_female)) / (n_periods - OM_test.L + 1)) ^ 2)

# alternative \hat{\sigma}_e^2
OM_sigma.hat_e.sq_alt_both <- sum(OM_ehat_t_both ^ 2) / n_periods + 2 / n_periods *
  sum(OM_ehat_t1_both * OM_ehat_t0_both)
OM_sigma.hat_e.sq_alt_male <- sum(OM_ehat_t_male ^ 2) / n_periods + 2 / n_periods *
  sum(OM_ehat_t1_male * OM_ehat_t0_male)
OM_sigma.hat_e.sq_alt_female <- sum(OM_ehat_t_female ^ 2) / n_periods +
  2 / n_periods * sum(OM_ehat_t1_female * OM_ehat_t0_female)

# unit-root test: Chi-square test statistic and p-value
OM_unitrootteststatistic_both <- OM_mu.adjusted_both ^ 2 * n_periods ^ 3 *
  (OM_phi_both - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_both)
OM_unitrootteststatistic_male <- OM_mu.adjusted_male ^ 2 * n_periods ^ 3 *
  (OM_phi_male - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_male)
OM_unitrootteststatistic_female <- OM_mu.adjusted_female ^ 2 * n_periods ^ 3 *
  (OM_phi_female - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_female)

OM_unitrootteststatistic_alt_both <- OM_mu.adjusted_both ^ 2 * n_periods ^ 3 *
  (OM_phi_both - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_alt_both)
OM_unitrootteststatistic_alt_male <- OM_mu.adjusted_male ^ 2 * n_periods ^ 3 *
  (OM_phi_male - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_alt_male)
OM_unitrootteststatistic_alt_female <- OM_mu.adjusted_female ^ 2 * n_periods ^ 3 *
  (OM_phi_female - 1) ^ 2 / (12 * OM_sigma.hat_e.sq_alt_female)

OM_unitroottest_p.value_both <- pchisq(OM_unitrootteststatistic_both, df = 1,
                                       lower.tail = FALSE)
OM_unitroottest_p.value_male <- pchisq(OM_unitrootteststatistic_male, df = 1,
                                       lower.tail = FALSE)
OM_unitroottest_p.value_female <- pchisq(OM_unitrootteststatistic_female, df = 1,
                                         lower.tail = FALSE)

OM_unitroottest_alt_p.value_both <- pchisq(OM_unitrootteststatistic_alt_both,
                                           df = 1, lower.tail = FALSE)
OM_unitroottest_alt_p.value_male <- pchisq(OM_unitrootteststatistic_alt_male,
                                           df = 1, lower.tail = FALSE)
OM_unitroottest_alt_p.value_female <- pchisq(OM_unitrootteststatistic_alt_female,
                                             df = 1, lower.tail = FALSE)
