# Econometrics with R
# Chapter 06 Regression Models with Multiple Regressors ------------------------
library(AER)
library(MASS)
library(mvtnorm)
library(tidyverse)
library(ggplot2)


# 6.1 Omitted Variable Bias ----------------------------------------------------

# load the AER package
# library(AER)

# load the data set
data(CASchools)   
CASchools <- as_tibble(CASchools)

# define variables
# CASchools$STR <- CASchools$students/CASchools$teachers       
# CASchools$score <- (CASchools$read + CASchools$math)/2
CASchools <- CASchools %>% 
  mutate(STR = students/teachers,
         score = (read + math)/2)

# compute correlations
cor(CASchools$STR, CASchools$score)
#> [1] -0.2263627
cor(CASchools$STR, CASchools$english)
#> [1] 0.1876424


# estimate both regression models
mod <- lm(score ~ STR, data = CASchools) 
mult.mod <- lm(score ~ STR + english, data = CASchools)

# print the results to the console
mod
#> 
#> Call:
#> lm(formula = score ~ STR, data = CASchools)
#> 
#> Coefficients:
#> (Intercept)          STR  
#>      698.93        -2.28
mult.mod
#> 
#> Call:
#> lm(formula = score ~ STR + english, data = CASchools)
#> 
#> Coefficients:
#> (Intercept)          STR      english  
#>    686.0322      -1.1013      -0.6498


# 6.2 The Multiple Regression Model --------------------------------------------

summary(mult.mod)$coef
#>                Estimate Std. Error    t value      Pr(>|t|)
#> (Intercept) 686.0322445 7.41131160  92.565565 3.871327e-280
#> STR          -1.1012956 0.38027827  -2.896026  3.978059e-03
#> english      -0.6497768 0.03934254 -16.515882  1.657448e-47


# 6.3 Measures of Fit in Multiple Regression -----------------------------------

summary(mult.mod)
#> 
#> Call:
#> lm(formula = score ~ STR + english, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -48.845 -10.240  -0.308   9.815  43.461 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 686.03224    7.41131  92.566  < 2e-16 ***
#> STR          -1.10130    0.38028  -2.896  0.00398 ** 
#> english      -0.64978    0.03934 -16.516  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.46 on 417 degrees of freedom
#> Multiple R-squared:  0.4264, Adjusted R-squared:  0.4237 
#> F-statistic:   155 on 2 and 417 DF,  p-value: < 2.2e-16


# define the components
n <- nrow(CASchools)                            # number of observations (rows)
k <- 2                                          # number of regressors

y_mean <- mean(CASchools$score)                 # mean of avg. test-scores

SSR <- sum(residuals(mult.mod)^2)               # sum of squared residuals
TSS <- sum((CASchools$score - y_mean )^2)       # total sum of squares
ESS <- sum((fitted(mult.mod) - y_mean)^2)       # explained sum of squares

# compute the measures

SER <- sqrt(1/(n-k-1) * SSR)                    # standard error of the regression
Rsq <- 1 - (SSR / TSS)                          # R^2
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/TSS          # adj. R^2

# print the measures to the console
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)
#>        SER         R2     Adj.R2 
#> 14.4644831  0.4264315  0.4236805


# 6.4 OLS Assumptions in Multiple Regression -----------------------------------

# define the fraction of English learners        
# CASchools$FracEL <- CASchools$english / 100
CASchools <- CASchools %>% 
  mutate(FracEL = english / 100)

# estimate the model
mult.mod <- lm(score ~ STR + english + FracEL, data = CASchools) 

# obtain a summary of the model
summary(mult.mod)                                                 
#> 
#> Call:
#> lm(formula = score ~ STR + english + FracEL, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -48.845 -10.240  -0.308   9.815  43.461 
#> 
#> Coefficients: (1 not defined because of singularities)
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 686.03224    7.41131  92.566  < 2e-16 ***
#> STR          -1.10130    0.38028  -2.896  0.00398 ** 
#> english      -0.64978    0.03934 -16.516  < 2e-16 ***
#> FracEL             NA         NA      NA       NA    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.46 on 417 degrees of freedom
#> Multiple R-squared:  0.4264, Adjusted R-squared:  0.4237 
#> F-statistic:   155 on 2 and 417 DF,  p-value: < 2.2e-16


# if STR smaller 12, NS = 0, else NS = 1
# CASchools$NS <- ifelse(CASchools$STR < 12, 0, 1)
CASchools <- CASchools %>% 
  mutate(NS = ifelse(STR < 12, 0, 1))

# estimate the model
mult.mod <- lm(score ~ computer + english + NS, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                  
#> 
#> Call:
#> lm(formula = score ~ computer + english + NS, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -49.492  -9.976  -0.778   8.761  43.798 
#> 
#> Coefficients: (1 not defined because of singularities)
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 663.704837   0.984259 674.319  < 2e-16 ***
#> computer      0.005374   0.001670   3.218  0.00139 ** 
#> english      -0.708947   0.040303 -17.591  < 2e-16 ***
#> NS                  NA         NA      NA       NA    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.43 on 417 degrees of freedom
#> Multiple R-squared:  0.4291, Adjusted R-squared:  0.4263 
#> F-statistic: 156.7 on 2 and 417 DF,  p-value: < 2.2e-16


table(CASchools$NS)
#> 
#>   1 
#> 420


# set seed for reproducibility
set.seed(1)

# generate artificial data on location
# CASchools$direction <- sample(c("West", "North", "South", "East"), 
#                               420, 
#                               replace = T)
CASchools <- CASchools %>% 
  mutate(direction = sample(c("West", "North", "South", "East"), 
                            420, 
                            replace = T))

# estimate the model
mult.mod <- lm(score ~ STR + english + direction, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                 
#> 
#> Call:
#> lm(formula = score ~ STR + english + direction, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -49.603 -10.175  -0.484   9.524  42.830 
#> 
#> Coefficients:
#>                 Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    684.80477    7.54130  90.807  < 2e-16 ***
#> STR             -1.08873    0.38153  -2.854  0.00454 ** 
#> english         -0.65597    0.04018 -16.325  < 2e-16 ***
#> directionNorth   1.66314    2.05870   0.808  0.41964    
#> directionSouth   0.71619    2.06321   0.347  0.72867    
#> directionWest    1.79351    1.98174   0.905  0.36598    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.5 on 414 degrees of freedom
#> Multiple R-squared:  0.4279, Adjusted R-squared:  0.421 
#> F-statistic: 61.92 on 5 and 414 DF,  p-value: < 2.2e-16


# Percentage of english speakers 
# CASchools$PctES <- 100 - CASchools$english
CASchools <- CASchools %>% 
  mutate(PctES = 100 - english)

# estimate the model
mult.mod <- lm(score ~ STR + english + PctES, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                 
#> 
#> Call:
#> lm(formula = score ~ STR + english + PctES, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -48.845 -10.240  -0.308   9.815  43.461 
#> 
#> Coefficients: (1 not defined because of singularities)
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 686.03224    7.41131  92.566  < 2e-16 ***
#> STR          -1.10130    0.38028  -2.896  0.00398 ** 
#> english      -0.64978    0.03934 -16.516  < 2e-16 ***
#> PctES              NA         NA      NA       NA    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.46 on 417 degrees of freedom
#> Multiple R-squared:  0.4264, Adjusted R-squared:  0.4237 
#> F-statistic:   155 on 2 and 417 DF,  p-value: < 2.2e-16


# load packages
# library(MASS)
# library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
    
    # for cov(X_1,X_2) = 0.25
    X <- rmvnorm(n, c(0, 0), sigma = cbind(c(10, 2.5), c(2.5, 10)))
    u <- rnorm(n, sd = 5)
    Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
    coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
    
    # for cov(X_1,X_2) = 0.85
    X <- rmvnorm(n, c(0, 0), sigma = cbind(c(10, 8.5), c(8.5, 10)))
    Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
    coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
    
}

# obtain variance estimates
diag(var(coefs1))
#> hat_beta_1 hat_beta_2 
#> 0.05674375 0.05712459
diag(var(coefs2))
#> hat_beta_1 hat_beta_2 
#>  0.1904949  0.1909056


# 6.5 The Distribution of the OLS Estimators in Multiple Regression

# set sample size
n <- 50

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

# set seed for reproducibility
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
    
    X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
    u <- rnorm(n, sd = 5)
    Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
    coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
    
}

# compute density estimate
kde <- kde2d(coefs[, 1], coefs[, 2])

# plot density estimate
persp(kde,
      theta = 310,
      phi = 30,
      xlab = "beta_1",
      ylab = "beta_2",
      zlab = "Est. Density",
      main = "2D Kernel Density Estimate")


# estimate the correlation between estimators
cor(coefs[, 1], coefs[, 2])
#> [1] -0.2503028


# 6.6 Exercises ----------------------------------------------------------------

# 1. The Boston Housing Data Set
# attach both packages and load the data set
data("Boston")
Boston <- as_tibble(Boston)

# obtain an overview over the data set
summary(Boston)
#      crim                zn             indus            chas        
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
# 1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
#      nox               rm             age              dis        
# Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
# 1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
# Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
# Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
# 3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
# Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
#      rad              tax           ptratio          black       
# Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
# 1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
# Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
# Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
# 3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
# Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
#     lstat            medv      
# Min.   : 1.73   Min.   : 5.00  
# 1st Qu.: 6.95   1st Qu.:17.02  
# Median :11.36   Median :21.20  
# Mean   :12.65   Mean   :22.53  
# 3rd Qu.:16.95   3rd Qu.:25.00  
# Max.   :37.97   Max.   :50.00 

str(Boston)
# tibble [506 × 14] (S3: tbl_df/tbl/data.frame)
# $ crim   : num [1:506] 0.00632 0.02731 0.02729 0.03237 0.06905 ...
# $ zn     : num [1:506] 18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
# $ indus  : num [1:506] 2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
# $ chas   : int [1:506] 0 0 0 0 0 0 0 0 0 0 ...
# $ nox    : num [1:506] 0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
# $ rm     : num [1:506] 6.58 6.42 7.18 7 7.15 ...
# $ age    : num [1:506] 65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
# $ dis    : num [1:506] 4.09 4.97 4.97 6.06 6.06 ...
# $ rad    : int [1:506] 1 2 2 3 3 3 5 5 5 5 ...
# $ tax    : num [1:506] 296 242 242 222 222 222 311 311 311 311 ...
# $ ptratio: num [1:506] 15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
# $ black  : num [1:506] 397 397 393 395 397 ...
# $ lstat  : num [1:506] 4.98 9.14 4.03 2.94 5.33 ...
# $ medv   : num [1:506] 24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...

head(Boston)
# # A tibble: 6 × 14
#      crim    zn indus  chas   nox    rm   age   dis   rad   tax ptratio black
#     <dbl> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl> <dbl>
# 1 0.00632    18  2.31     0 0.538  6.58  65.2  4.09     1   296    15.3  397.
# 2 0.0273      0  7.07     0 0.469  6.42  78.9  4.97     2   242    17.8  397.
# 3 0.0273      0  7.07     0 0.469  7.18  61.1  4.97     2   242    17.8  393.
# 4 0.0324      0  2.18     0 0.458  7.00  45.8  6.06     3   222    18.7  395.
# 5 0.0690      0  2.18     0 0.458  7.15  54.2  6.06     3   222    18.7  397.
# 6 0.0298      0  2.18     0 0.458  6.43  58.7  6.06     3   222    18.7  394.
# # ℹ 2 more variables: lstat <dbl>, medv <dbl>


# estimate the simple regression model
bh_mod <- lm(medv ~ lstat, data = Boston)

# print the summary to the console
summary(bh_mod)
# Call:
# lm(formula = medv ~ lstat, data = Boston)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -15.168  -3.990  -1.318   2.034  24.500 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
# lstat       -0.95005    0.03873  -24.53   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

coeftest(bh_mod, vcov. = vcovHC)
# t test of coefficients:
#     
#             Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 34.55384    0.75857  45.552 < 2.2e-16 ***
# lstat       -0.95005    0.05008 -18.971 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 2. A Multiple Regression Model of Housing Prices I
# conduct the regression
mod <- lm(medv ~ lstat + age + crim, data = Boston)

# obtain a robust coefficient summary
coeftest(mod, vcov. = vcovHC)
# t test of coefficients:
#     
#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 32.828045   0.751505  43.6831  < 2e-16 ***
# lstat       -0.994091   0.083058 -11.9686  < 2e-16 ***
# age          0.037647   0.016930   2.2236  0.02662 *  
# crim        -0.082622   0.029733  -2.7788  0.00566 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# compare both coefficients of determination
R2_res <- summary(bh_mod)$r.squared

R2_unres <- summary(mod)$r.squared
R2_unres < R2_res
# [1] FALSE


# 3. A Multiple Regression Model of Housing Prices II
# correction Factor
n <- nrow(Boston)
k <- 3
CF <- (n - 1) / (n - k - 1)

# obtain both R^2 and the adj. R^2
R2 <- summary(mod)$r.squared
adj_R2 <- summary(mod)$adj.r.squared
R2
#> [1] 0.5532892
adj_R2
#> [1] 0.5532892

# check that the adj. R^2 can be computed as stated above
1 - (1 - R2) * CF == adj_R2
#> [1] TRUE


# 4. A Fully-Fledged Model for Housing Values?
# run a regression of medv on all remaining variables in the Boston data set
full_mod <- lm(medv ~ ., data = Boston)

# obtain a robust summary of the coefficients
coeftest(full_mod, vcov. = vcovHC, type = "HC1")
# t test of coefficients:
#     
#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)  3.6459e+01  8.0010e+00  4.5569 6.558e-06 ***
# crim        -1.0801e-01  2.8944e-02 -3.7317 0.0002124 ***
# zn           4.6420e-02  1.3765e-02  3.3722 0.0008043 ***
# indus        2.0559e-02  5.0380e-02  0.4081 0.6834006    
# chas         2.6867e+00  1.2938e+00  2.0766 0.0383600 *  
# nox         -1.7767e+01  3.7858e+00 -4.6930 3.495e-06 ***
# rm           3.8099e+00  8.4490e-01  4.5093 8.142e-06 ***
# age          6.9222e-04  1.6464e-02  0.0420 0.9664807    
# dis         -1.4756e+00  2.1471e-01 -6.8724 1.918e-11 ***
# rad          3.0605e-01  6.1436e-02  4.9816 8.744e-07 ***
# tax         -1.2335e-02  2.6909e-03 -4.5838 5.798e-06 ***
# ptratio     -9.5275e-01  1.1744e-01 -8.1126 3.985e-15 ***
# black        9.3117e-03  2.6786e-03  3.4763 0.0005534 ***
# lstat       -5.2476e-01  9.9650e-02 -5.2660 2.087e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# what is the R^2 of the model?
summary(full_mod)$adj.r.squared
#> [1] 0.7337897


# 5. Model Selection
# find the model which fits the data better than full_mod
# this solution is a bit technical but efficient

# loop estimation of models
l <- list()
for (i in 1:13) {
    d <- Boston[, -i]
    # save each adj. R^2 as a list entry in l
    l[[i]] <- summary(lm(medv ~., data=d))$adj.r.squared 
}

# assign variable names to the list entries
names(l) <- names(Boston[, 1:13]) 

# select the variable whose omission leads to the highest improvement in adj. R^2
which.max(l) # 7th column this is "age"

# hence a model that fits the data better is
better_model <- lm(medv ~., data = Boston[, -7])
summary(better_model)$adj.r.squared
#> [1] 0.7343282
