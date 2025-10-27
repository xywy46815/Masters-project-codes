#SET PATH
setwd('C:/Users/taowu/Desktop/all')


# DATA IMPORT
## USA data, 5 x 1, male, female and total data
## http://www.mortality.org/cgi-bin/hmd/country.php?cntr=USA&level=1
## load raw data files
#比利时
lifetbl_both_belgium <- read.table('belgium_5x1.txt', skip = 2, header = TRUE,
                                   stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
library(dplyr)
library(stringr)
lifetbl_both_belgium <- mutate(lifetbl_both_belgium, Age0 = as.numeric(str_extract(lifetbl_both_belgium$Age,
                                                                                   '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_belgium <- lifetbl_both_belgium[lifetbl_both_belgium$Age0 >= 25 & lifetbl_both_belgium$Age0 < 75, ]
lifetbl_both_belgium <- lifetbl_both_belgium[lifetbl_both_belgium$Year >= 1950 & lifetbl_both_belgium$Year < 2020, ]
## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_belgium$Age0)) # n_agegroups = 10
## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_belgium$Year)) 
t_last <- last(lifetbl_both_belgium$Year)
t_first <- first(lifetbl_both_belgium$Year)
## create mx
lifetbl_both_belgium$mx <- as.numeric(lifetbl_both_belgium$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_belgium_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_belgium <- lifetbl_both_belgium %>% filter(Year == year)
  z_belgium<- sum(log(lifetbl_both_filtered_belgium$mx), na.rm = TRUE) / 10
  z_belgium_vector[i] <- z_belgium
  print(z_belgium)
}
### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_belgium_", i, ": ",z_belgium_vector[i], "\n"))
}


#法国
lifetbl_both_france <- read.table('france.txt', skip = 2, header = TRUE,
                                  stringsAsFactors = FALSE)
#DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_france <- mutate(lifetbl_both_france, Age0 = as.numeric(str_extract(lifetbl_both_france$Age,
                                                                                 '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_france <- lifetbl_both_france[lifetbl_both_france$Age0 >= 25 & lifetbl_both_france$Age0 < 75, ]

lifetbl_both_france <- lifetbl_both_france[lifetbl_both_france$Year >= 1950 & lifetbl_both_france$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_france$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_france$Year)) 
t_last <- last(lifetbl_both_france$Year)
t_first <- first(lifetbl_both_france$Year)
## create mx
lifetbl_both_france$mx <- as.numeric(lifetbl_both_france$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_france_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  lifetbl_both_filtered_france <- lifetbl_both_france %>% filter(Year == year)
  z_france <- sum(log(lifetbl_both_filtered_france$mx), na.rm = TRUE) / 10
  z_france_vector[i] <- z_france
  print(z_france)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_france_", i, ": ", z_france_vector[i], "\n"))
}





#意大利
lifetbl_both_Italy <- read.table('Italy.txt', skip = 2, header = TRUE,
                                 stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Italy <- mutate(lifetbl_both_Italy, Age0 = as.numeric(str_extract(lifetbl_both_Italy$Age,
                                                                               '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Italy <- lifetbl_both_Italy[lifetbl_both_Italy$Age0 >= 25 & lifetbl_both_Italy$Age0 < 75, ]

lifetbl_both_Italy <- lifetbl_both_Italy[lifetbl_both_Italy$Year >= 1950 & lifetbl_both_Italy$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Italy$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Italy$Year)) 
t_last <- last(lifetbl_both_Italy$Year)
t_first <- first(lifetbl_both_Italy$Year)
## create mx
lifetbl_both_Italy$mx <- as.numeric(lifetbl_both_Italy$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Italy_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Italy <- lifetbl_both_Italy %>% filter(Year == year)
  z_Italy <- sum(log(lifetbl_both_filtered_Italy$mx), na.rm = TRUE) / 10
  z_Italy_vector[i] <- z_Italy
  print(z_Italy)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Italy_", i, ": ", z_Italy_vector[i], "\n"))
}


#荷兰
lifetbl_both_Netherlands <- read.table('Netherlands.txt', skip = 2, header = TRUE,
                                       stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Netherlands <- mutate(lifetbl_both_Netherlands, Age0 = as.numeric(str_extract(lifetbl_both_Netherlands$Age,
                                                                                           '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Netherlands <- lifetbl_both_Netherlands[lifetbl_both_Netherlands$Age0 >= 25 & lifetbl_both_Netherlands$Age0 < 75, ]

lifetbl_both_Netherlands <- lifetbl_both_Netherlands[lifetbl_both_Netherlands$Year >= 1950 & lifetbl_both_Netherlands$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Netherlands$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Netherlands$Year)) 
t_last <- last(lifetbl_both_Netherlands$Year)
t_first <- first(lifetbl_both_Netherlands$Year)
## create mx
lifetbl_both_Netherlands$mx <- as.numeric(lifetbl_both_Netherlands$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Netherlands_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Netherlands <- lifetbl_both_Netherlands %>% filter(Year == year)
  z_Netherlands <- sum(log(lifetbl_both_filtered_Netherlands$mx), na.rm = TRUE) / 10
  z_Netherlands_vector[i] <- z_Netherlands
  print(z_Netherlands)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Netherlands_", i, ": ", z_Netherlands_vector[i], "\n"))
}


#西班牙
lifetbl_both_spain <- read.table('spain.txt', skip = 2, header = TRUE,
                                 stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_spain <- mutate(lifetbl_both_spain, Age0 = as.numeric(str_extract(lifetbl_both_spain$Age,
                                                                               '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_spain <- lifetbl_both_spain[lifetbl_both_spain$Age0 >= 25 & lifetbl_both_spain$Age0 < 75, ]

lifetbl_both_spain <- lifetbl_both_spain[lifetbl_both_spain$Year >= 1950 & lifetbl_both_spain$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_spain$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_spain$Year)) 
t_last <- last(lifetbl_both_spain$Year)
t_first <- first(lifetbl_both_spain$Year)
## create mx
lifetbl_both_spain$mx <- as.numeric(lifetbl_both_spain$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_spain_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_spain <- lifetbl_both_spain %>% filter(Year == year)
  z_spain <- sum(log(lifetbl_both_filtered_spain$mx), na.rm = TRUE) / 10
  z_spain_vector[i] <- z_spain
  print(z_spain)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_spain_", i, ": ", z_spain_vector[i], "\n"))
}

#瑞士
lifetbl_both_Switzerland <- read.table('Switzerland.txt', skip = 2, header = TRUE,
                                       stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Switzerland <- mutate(lifetbl_both_Switzerland, Age0 = as.numeric(str_extract(lifetbl_both_Switzerland$Age,
                                                                                           '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Switzerland <- lifetbl_both_Switzerland[lifetbl_both_Switzerland$Age0 >= 25 & lifetbl_both_Switzerland$Age0 < 75, ]

lifetbl_both_Switzerland <- lifetbl_both_Switzerland[lifetbl_both_Switzerland$Year >= 1950 & lifetbl_both_Switzerland$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Switzerland$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Switzerland$Year)) 
t_last <- last(lifetbl_both_Switzerland$Year)
t_first <- first(lifetbl_both_Switzerland$Year)
## create mx
lifetbl_both_Switzerland$mx <- as.numeric(lifetbl_both_Switzerland$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Switzerland_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Switzerland <- lifetbl_both_Switzerland %>% filter(Year == year)
  z_Switzerland <- sum(log(lifetbl_both_filtered_Switzerland$mx), na.rm = TRUE) / 10
  z_Switzerland_vector[i] <- z_Switzerland
  print(z_Switzerland)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Switzerland_", i, ": ", z_Switzerland_vector[i], "\n"))
}

#挪威
lifetbl_both_Norway <- read.table('Norway.txt', skip = 2, header = TRUE,
                                  stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
library(dplyr)
library(stringr)
lifetbl_both_Norway <- mutate(lifetbl_both_Norway, Age0 = as.numeric(str_extract(lifetbl_both_Norway$Age,
                                                                                 '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Norway <- lifetbl_both_Norway[lifetbl_both_Norway$Age0 >= 25 & lifetbl_both_Norway$Age0 < 75, ]
lifetbl_both_Norway <- lifetbl_both_Norway[lifetbl_both_Norway$Year >= 1950 & lifetbl_both_Norway$Year < 2020, ]
## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Norway$Age0)) # n_agegroups = 10
## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Norway$Year)) 
t_last <- last(lifetbl_both_Norway$Year)
t_first <- first(lifetbl_both_Norway$Year)
## create mx
lifetbl_both_Norway$mx <- as.numeric(lifetbl_both_Norway$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Norway_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Norway <- lifetbl_both_Norway %>% filter(Year == year)
  z_Norway<- sum(log(lifetbl_both_filtered_Norway$mx), na.rm = TRUE) / 10
  z_Norway_vector[i] <- z_Norway
  print(z_Norway)
}
### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Norway_", i, ": ",z_Norway_vector[i], "\n"))
}








#瑞典
lifetbl_both_Sweden <- read.table('Sweden.txt', skip = 2, header = TRUE,
                                  stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Sweden <- mutate(lifetbl_both_Sweden, Age0 = as.numeric(str_extract(lifetbl_both_Sweden$Age,
                                                                                 '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Sweden <- lifetbl_both_Sweden[lifetbl_both_Sweden$Age0 >= 25 & lifetbl_both_Sweden$Age0 < 75, ]

lifetbl_both_Sweden <- lifetbl_both_Sweden[lifetbl_both_Sweden$Year >= 1950 & lifetbl_both_Sweden$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Sweden$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Sweden$Year)) 
t_last <- last(lifetbl_both_Sweden$Year)
t_first <- first(lifetbl_both_Sweden$Year)
## create mx
lifetbl_both_Sweden$mx <- as.numeric(lifetbl_both_Sweden$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Sweden_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Sweden <- lifetbl_both_Sweden %>% filter(Year == year)
  z_Sweden <- sum(log(lifetbl_both_filtered_Sweden$mx), na.rm = TRUE) / 10
  z_Sweden_vector[i] <- z_Sweden
  print(z_Sweden)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Sweden_", i, ": ", z_Sweden_vector[i], "\n"))
}


#芬兰
lifetbl_both_Finland <- read.table('Finland_5x1.txt', skip = 2, header = TRUE,
                                   stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Finland <- mutate(lifetbl_both_Finland, Age0 = as.numeric(str_extract(lifetbl_both_Finland$Age,
                                                                                   '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Finland <- lifetbl_both_Finland[lifetbl_both_Finland$Age0 >= 25 & lifetbl_both_Finland$Age0 < 75, ]

lifetbl_both_Finland <- lifetbl_both_Finland[lifetbl_both_Finland$Year >= 1950 & lifetbl_both_Finland$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Finland$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Finland$Year)) 
t_last <- last(lifetbl_both_Finland$Year)
t_first <- first(lifetbl_both_Finland$Year)
## create mx
lifetbl_both_Finland$mx <- as.numeric(lifetbl_both_Finland$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Finland_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Finland <- lifetbl_both_Finland %>% filter(Year == year)
  z_Finland <- sum(log(lifetbl_both_filtered_Finland$mx), na.rm = TRUE) / 10
  z_Finland_vector[i] <- z_Finland
  print(z_Finland)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Finland_", i, ": ", z_Finland_vector[i], "\n"))
}


#丹麦
lifetbl_both_Denmark <- read.table('Denmark_5x1.txt', skip = 2, header = TRUE,
                                   stringsAsFactors = FALSE)
# DATA CLEANUP
## replace 110+ with 110, change data format to numeric
lifetbl_both_Denmark <- mutate(lifetbl_both_Denmark, Age0 = as.numeric(str_extract(lifetbl_both_Denmark$Age,
                                                                                   '[0-9]+')))
## focus on ages of 25 - 74
lifetbl_both_Denmark <- lifetbl_both_Denmark[lifetbl_both_Denmark$Age0 >= 25 & lifetbl_both_Denmark$Age0 < 75, ]

lifetbl_both_Denmark <- lifetbl_both_Denmark[lifetbl_both_Denmark$Year >= 1950 & lifetbl_both_Denmark$Year < 2020, ]

## n_agegroups: number of age groups (M in paper)
n_agegroups <- length(unique(lifetbl_both_Denmark$Age0)) # n_agegroups = 10

## n_periods: number of years observed (T in paper)
n_periods <- length(unique(lifetbl_both_Denmark$Year)) 
t_last <- last(lifetbl_both_Denmark$Year)
t_first <- first(lifetbl_both_Denmark$Year)
## create mx
lifetbl_both_Denmark$mx <- as.numeric(lifetbl_both_Denmark$Total)    
## culculate Z1t、Z2t
### define year coverage
years <- t_first :t_last
### save the result
z_Denmark_vector <- numeric(length(years))
### loop for every year
for (i in 1:length(years)) {
  year <- years[i]
  ### man
  lifetbl_both_filtered_Denmark <- lifetbl_both_Denmark %>% filter(Year == year)
  z_Denmark <- sum(log(lifetbl_both_filtered_Denmark$mx), na.rm = TRUE) / 10
  z_Denmark_vector[i] <- z_Denmark
  print(z_Denmark)
}

### print result of per year
for (i in 1:length(years)) {
  cat(paste0("z_Denmark_", i, ": ", z_Denmark_vector[i], "\n"))
}




#z_matrix
# 使用 cbind() 将它们按列绑定为矩阵
z_matrix <- cbind(z_Norway_vector ,z_Denmark_vector,z_Italy_vector ,z_Finland_vector,z_Switzerland_vector)#)#z_Netherlands_vector,z_france_vector,z_spain_vector,)

### regression

set.seed(42)
t <- 0:(t_last - t_first)
print(t)
df <- data.frame(t, x = z_matrix, y = z_Sweden_vector)#) #z_belgium_vector)

model <- lm(y ~ t + ., data = df)

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


# 残差绘制
plot(v, pch = NA, xlim = c(1, length(v) * 1), ylim = range(v) * 10, xlab = "T", ylab = "Residual")  
lines(v, lwd = 1.25)


# 残差的adf检验

library(tseries)
adf_test <- adf.test(v)
print(adf_test)





