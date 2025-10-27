#SET PATH
setwd('C:/Users/taowu/Desktop/Learning Files/mortality/单位根检验/意大利')


# DATA IMPORT

## USA data, 5 x 1, male, female and total data
## http://www.mortality.org/cgi-bin/hmd/country.php?cntr=USA&level=1
## load raw data files
lifetbl_both <- read.table('Italy.txt', skip = 2, header = TRUE,
                           stringsAsFactors = FALSE)
lifetbl_male <- lifetbl_both[, -which(names(lifetbl_both) == "Female")]
lifetbl_male <- lifetbl_male[, -which(names(lifetbl_male) == "Total")]

lifetbl_female <- lifetbl_both[, -which(names(lifetbl_both) == "Male")]
lifetbl_female <- lifetbl_female[, -which(names(lifetbl_female) == "Total")]


# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
library(dplyr)
library(stringr)

lifetbl_both <- mutate(lifetbl_both, Age0 = as.numeric(str_extract(lifetbl_both$Age,
                                                                   '[0-9]+')))
lifetbl_male <- mutate(lifetbl_male, Age0 = as.numeric(str_extract(lifetbl_male$Age,
                                                                   '[0-9]+')))
lifetbl_female <- mutate(lifetbl_female, Age0 =
                           as.numeric(str_extract(lifetbl_female$Age, '[0-9]+')))

## focus on ages of 25 - 74
lifetbl_both <- lifetbl_both[lifetbl_both$Age0 >= 25 & lifetbl_both$Age0 < 75, ]
lifetbl_male <- lifetbl_male[lifetbl_male$Age0 >= 25 & lifetbl_male$Age0 < 75, ]
lifetbl_female <- lifetbl_female[lifetbl_female$Age0 >= 25 & lifetbl_female$Age0 < 75, ]

lifetbl_both <- lifetbl_both[#lifetbl_both$Year >=1919 &
  lifetbl_both$Year < 2020, ]
lifetbl_male <- lifetbl_male[#lifetbl_male$Year >= 1919 &
  lifetbl_male$Year < 2020, ]
lifetbl_female <- lifetbl_female[#lifetbl_female$Year>= 1919 & 
  lifetbl_female$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both$Year)) 
t_last <- last(lifetbl_both$Year)
t_first <- first(lifetbl_both$Year)


## create mx
lifetbl_both$mx <- as.numeric(lifetbl_both$Total)    
lifetbl_male$mx <- as.numeric(lifetbl_male$Male)  
lifetbl_female$mx <- as.numeric(lifetbl_female$Female) 

## culculate Z1t、Z2t

### define year coverage
years <- t_first :t_last

### save the result
z_male_vector <- numeric(length(years))
z_female_vector <- numeric(length(years))

### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  
  ### man
  lifetbl_male_filtered <- lifetbl_male %>% filter(Year == year)
  z_male <- sum(log(lifetbl_male_filtered$mx), na.rm = TRUE) / 10
  z_male_vector[i] <- z_male
  print(z_male)
  
  ### woman
  lifetbl_female_filtered <- lifetbl_female %>% filter(Year == year)
  z_female <- sum(log(lifetbl_female_filtered$mx), na.rm = TRUE) / 10
  z_female_vector[i] <- z_female
  print(z_female)
}

### print result
print("Results for males:")
print(z_male_vector)

print("Results for females:")
print(z_female_vector)

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z1", i, ": ", z_male_vector[i], "\n"))
  cat(paste0("z2", i, ": ", z_female_vector[i], "\n"))
}



### regression

set.seed(42)
t <- 0:(t_last - t_first)
print(t)
df <- data.frame(t, x = z_female_vector, y = z_male_vector)

model <- lm(y ~ t + x, data = df)

print(summary(model)$coefficients)
cat("lm() Alpha (Intercept):", coef(model)[1], "\n")
cat("lm() Beta (t):", coef(model)[2], "\n")
cat("lm() Gamma (x):", coef(model)[3], "\n")

## culculate vt
t_num <- length(t)
v <- model$residuals
# drop first element
vt <- v[-1]
# drop last element
vt_1 <- v[-length(v)]

# culculate numerator
numerator <- sum(vt * vt_1)

#numerator_another = 0
#for (i in 3:t_num) {
 # numerator_another <-  numerator_another +v[i]*v[i-1]
 # return(numerator_another)
#}

# culculate denominator
denominator <- sum(vt_1^2)

#denominator_another = 0
#for (i in 3:t_num) {
  #denominator_another <-  denominator_another +v[i-1]*v[i-1]
  #return(denominator_another)
#}



# culculate phi_hat
phi_hat <- numerator/denominator

# print result
cat("phi_hat =", phi_hat, "\n")



# drop the first and second elements
vt_star <- v[-c(1,2)]
# drop the last two elements
vt_2 <- head(v,-2)
# drop the first and the last elements
vt_1_star <- vt[-length(vt)]
# culculate numerator
numerator <- sum(vt_star * vt_2)

#numerator_another_star = 0
#for (i in 3:t_num) {
  #numerator_another_star <-  numerator_another_star +v[i]*v[i-2]
  #return(numerator_another_star)
#}
# clculate denominator
denominator <- sum(vt_1_star * vt_2)
#denominator_another_star = 0
#for (i in 3:t_num) {
  #denominator_another_star <-  denominator_another_star +v[i-1]*v[i-2]
 # return(denominator_another_star)
#}

# culculate phi_hat_star
phi_hat_star <- numerator / denominator
# print result
cat("phi_hat_star =", phi_hat_star, "\n")


#denominator behind

den_behind <- (1/(t_num * t_num)) * denominator

# the value of l
l <- t_num ** (1/4)
l <-floor(l)

# T*(fai-1)
t_f <- t_num * (phi_hat_star - 1)

# calculate w_sl
calculate_weights <- function(L) {
  w <- numeric(L)
  for (s in 1:L) {
    w[s] <- 1 - (s / (L + 1))
  }
  return(w)
}


w_h = calculate_weights(l)

# calculate lambda


calculate_lambda <- function(V, W, Time, L, Phi_star){
  
  lambda_T <- 0
  
  for (s in 2:L) {
    inner_sum <- 0
    for (t in (s+1):Time) {
      
      # 手动处理“0索引”的情况
      v_t_minus_1 <- ifelse(t-1 > 0, V[t-1], 0)
      v_t_minus_s <- ifelse(t-s > 0, V[t-s], 0)
      v_t_minus_s_minus_1 <- ifelse(t-s-1 > 0, V[t-s-1], 0)
      
      
      inner_sum <- inner_sum + 
      (V[t] - Phi_star * v_t_minus_1) * (v_t_minus_s - Phi_star * v_t_minus_s_minus_1)
      
      }
    lambda_T<-lambda_T + W[s]*inner_sum
  }
  lambda_T <- lambda_T/Time
  return(lambda_T)
}

V <- v
L <-l
Time <- t_num
Phi_star <- phi_hat_star
W=w_h
lambda_t <- calculate_lambda(V, W, Time, L, Phi_star)


# lam/den_behind
bhind <- lambda_t/den_behind

# Za
za <- t_num *(phi_hat_star - 1) - bhind
print(za)

