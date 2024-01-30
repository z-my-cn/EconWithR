# Econometrics with R
# Chapter 04 Linear Regression with One Regressor ------------------------------

library(AER)
library(MASS)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(readxl)

# 4.1 Simple Linear Regression -------------------------------------------------

# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635) 

# Print out sample data
STR
#> [1] 15.0 17.0 19.0 20.0 22.0 23.5 25.0
TestScore
#> [1] 680 640 670 660 630 660 635


# create a scatterplot of the data
# plot(TestScore ~ STR,ylab="Test Score",pch=20)

# add the systematic relationship to the plot
# abline(a = 713, b = -3)
ggplot() +
  geom_point(aes(x = STR, y = TestScore)) +
  geom_abline(intercept = 713, slope = -3) +
  labs(x = "STR", y = "Test Score")


# 4.2 Estimating the Coefficients of the Linear Regression Model ---------------

data("CASchools")
CASchools <- as_tibble(CASchools)
head(CASchools)
# # A tibble: 6 × 14
#   district school        county grades students teachers calworks lunch computer
#   <chr>    <chr>         <fct>  <fct>     <dbl>    <dbl>    <dbl> <dbl>    <dbl>
# 1 75119    Sunol Glen U… Alame… KK-08       195    10.9     0.510  2.04       67
# 2 61499    Manzanita El… Butte  KK-08       240    11.1    15.4   47.9       101
# 3 61549    Thermalito U… Butte  KK-08      1550    82.9    55.0   76.3       169
# 4 61457    Golden Feath… Butte  KK-08       243    14      36.5   77.0        85
# 5 61523    Palermo Unio… Butte  KK-08      1335    71.5    33.1   78.4       171
# 6 62042    Burrel Union… Fresno KK-08       137     6.40   12.3   87.0        25
# # ℹ 5 more variables: expenditure <dbl>, income <dbl>, english <dbl>,
# #   read <dbl>, math <dbl>


# compute STR and append it to CASchools
# CASchools$STR <- CASchools$students/CASchools$teachers 

# compute TestScore and append it to CASchools
# CASchools$score <- (CASchools$read + CASchools$math)/2   
CASchools <- CASchools %>% 
  mutate(STR = students/teachers,
         score = (read + math)/2)


# compute sample averages of STR and score
# avg_STR <- mean(CASchools$STR) 
# avg_score <- mean(CASchools$score)

# compute sample standard deviations of STR and score
# sd_STR <- sd(CASchools$STR) 
# sd_score <- sd(CASchools$score)

# set up a vector of percentiles and compute the quantiles 
# quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
# quant_STR <- quantile(CASchools$STR, quantiles)
# quant_score <- quantile(CASchools$score, quantiles)

# gather everything in a data.frame 
# DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
#                                   StandardDeviation = c(sd_STR, sd_score), 
#                                   quantile = rbind(quant_STR, quant_score))

# print the summary to the console
# DistributionSummary
#>               Average StandardDeviation quantile.10. quantile.25. quantile.40.
#> quant_STR    19.64043          1.891812      17.3486     18.58236     19.26618
#> quant_score 654.15655         19.053347     630.3950    640.05000    649.06999
#>             quantile.50. quantile.60. quantile.75. quantile.90.
#> quant_STR       19.72321      20.0783     20.87181     21.86741
#> quant_score    654.45000     659.4000    666.66249    678.85999
DistributionSummary <- CASchools %>% 
    select(STR, score) %>% 
    gather(key = "variable", value = "value") %>% 
    group_by(variable) %>% 
    summarise(Average = mean(value),
              StandardDeviation = sd(value),
              quantile.10. = quantile(value, 0.1),
              quantile.25. = quantile(value, 0.25),
              quantile.40. = quantile(value, 0.4),
              quantile.50. = quantile(value, 0.5),
              quantile.60. = quantile(value, 0.6),
              quantile.75. = quantile(value, 0.75),
              quantile.90. = quantile(value, 0.9))

DistributionSummary
# # A tibble: 2 × 10
#   variable Average StandardDeviation quantile.10. quantile.25. quantile.40.
#   <chr>      <dbl>             <dbl>        <dbl>        <dbl>        <dbl>
# 1 STR         19.6              1.89         17.3         18.6         19.3
# 2 score      654.              19.1         630.         640.         649. 
# # ℹ 4 more variables: quantile.50. <dbl>, quantile.60. <dbl>,
# #   quantile.75. <dbl>, quantile.90. <dbl>

# if the table is too wide to display in the console,
# you can use View() to view all the data.
view(DistributionSummary)


# plot(score ~ STR, 
#      data = CASchools,
#      main = "Scatterplot of Test Score and STR", 
#      xlab = "STR (X)",
#      ylab = "Test Score (Y)")
ggplot(CASchools, aes(x = STR, y = score)) +
  geom_point() +
  labs(x = "STR (X)", y = "Test Score (Y)", 
       title = "Scatterplot of Test Score and STR")

cor(CASchools$STR, CASchools$score)
#> [1] -0.2263627


# attach(CASchools) # allows to use the variables contained in CASchools directly

# I personally do not recommend using `attach()`, 
# because it is easy to cause variable name conflicts 
# and is not conducive to the readability of the code.

# compute beta_1_hat
# beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)
beta_1 <- CASchools %>% 
  summarise(
      beta_1 = sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)
  ) %>%
  pull(beta_1)

# compute beta_0_hat
# beta_0 <- mean(score) - beta_1 * mean(STR)
beta_0 <- CASchools %>% 
  summarise(
      beta_0 = mean(score) - beta_1 * mean(STR)
  ) %>%
  pull(beta_0)

# print the results to the console
beta_1
#> [1] -2.279808
beta_0
#> [1] 698.9329


# estimate the model and assign the result to linear_model
linear_model <- lm(score ~ STR, data = CASchools)

# print the standard output of the estimated lm object to the console 
linear_model
#> 
#> Call:
#> lm(formula = score ~ STR, data = CASchools)
#> 
#> Coefficients:
#> (Intercept)          STR  
#>      698.93        -2.28


# plot the data
# plot(score ~ STR, 
#      data = CASchools,
#      main = "Scatterplot of Test Score and STR", 
#      xlab = "STR (X)",
#      ylab = "Test Score (Y)",
#      xlim = c(10, 30),
#      ylim = c(600, 720))

# add the regression line
# abline(linear_model) 
ggplot(CASchools, aes(x = STR, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "STR (X)", y = "Test Score (Y)", 
       title = "Scatterplot of Test Score and STR")


# 4.3 Measures of Fit ----------------------------------------------------------

mod_summary <- summary(linear_model)
mod_summary
#> 
#> Call:
#> lm(formula = score ~ STR, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -47.727 -14.251   0.483  12.822  48.540 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 698.9329     9.4675  73.825  < 2e-16 ***
#> STR          -2.2798     0.4798  -4.751 2.78e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 18.58 on 418 degrees of freedom
#> Multiple R-squared:  0.05124,    Adjusted R-squared:  0.04897 
#> F-statistic: 22.58 on 1 and 418 DF,  p-value: 2.783e-06


# compute R^2 manually
SSR <- sum(mod_summary$residuals^2)
# TSS <- sum((score - mean(score))^2)
TSS <- sum((CASchools$score - mean(CASchools$score))^2)
R2 <- 1 - SSR/TSS

# print the value to the console
R2
#> [1] 0.05124009

# compute SER manually
n <- nrow(CASchools)
SER <- sqrt(SSR / (n-2))

# print the value to the console
SER
#> [1] 18.58097


# 4.4 The Least Squares Assumptions --------------------------------------------

# set a seed to make the results reproducible
set.seed(321)

# simulate the data 
X <- runif(50, min = -5, max = 5)
u <- rnorm(50, sd = 1)  

# the true relation  
Y <- X^2 + 2 * X + u                

# estimate a simple regression model 
mod_simple <- lm(Y ~ X)

# estimate a quadratic regression model
mod_quadratic <- lm( Y ~ X + I(X^2)) 

# predict using a quadratic model 
prediction <- predict(mod_quadratic, data.frame(X = sort(X)))

# plot the results
# plot( Y ~ X, col = "black", pch = 20, xlab = "X", ylab = "Y")
# abline( mod_simple, col = "blue",lwd=2)
#red line = incorrect linear regression (this violates the first OLS assumption)
# lines( sort(X), prediction,col="red",lwd=2)
# legend("topleft", 
#        legend = c("Simple Regression Model", 
#                   "Quadratic Model"),
#        cex = 1,
#        lty = 1,
#        col = c("blue","red"))
ggplot() +
  geom_point(aes(x = X, y = Y)) +
  geom_abline(aes(intercept = mod_simple$coefficients[1], 
                  slope = mod_simple$coefficients[2], 
                  color = "Simple Regression Model")) +
  geom_line(aes(x = sort(X), y = prediction, color = "Quadratic Model")) +
  scale_color_manual(name = NULL, 
                     values = c("Simple Regression Model" = "blue", 
                                "Quadratic Model" = "red")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# set seed
set.seed(123)

# generate a date vector
Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(5000, rep(NA, length(Date)-1))

# generate time series observations with random influences
for (t in 2:length(Date)) {
    
    X[t] <- -50 + 0.98 * X[t-1] + rnorm(n = 1, sd = 200)
    
}

#plot the results
# plot(x = Date, 
#      y = X, 
#      type = "l", 
#      col = "steelblue", 
#      ylab = "Workers", 
#      xlab = "Time",
#      lwd=2)
ggplot() +
  geom_line(aes(x = Date, y = X), color = "steelblue") +
  labs(x = "Time", y = "Workers")


# set seed
set.seed(123)

# generate the data
X <- sort(runif(10, min = 30, max = 70))
Y <- rnorm(10 , mean = 200, sd = 50)
Y[9] <- 2000

# fit model with outlier
fit <- lm(Y ~ X)

# fit model without outlier
fitWithoutOutlier <- lm(Y[-9] ~ X[-9])

# plot the results
# plot(Y ~ X,pch=20)
# abline(fit,lwd=2,col="blue")
# abline(fitWithoutOutlier, col = "red",lwd=2)
# legend("topleft", 
#        legend = c("Model with Outlier", 
#                   "Model without Outlier"),
#        cex = 1,
#        lty = 1,
#        col = c("blue","red"))
ggplot() +
  geom_point(aes(x = X, y = Y)) +
  geom_abline(aes(intercept = fit$coefficients[1], 
                  slope = fit$coefficients[2], 
                  color = "Model with Outlier")) +
  geom_abline(aes(intercept = fitWithoutOutlier$coefficients[1], 
                  slope = fitWithoutOutlier$coefficients[2], 
                  color = "Model without Outlier")) +
  scale_color_manual(name = NULL, 
                     values = c("Model with Outlier" = "blue", 
                                "Model without Outlier" = "red")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# 4.5 The Sampling Distribution of the OLS Estimator ---------------------------

# simulate data
N <- 100000
X <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

# population regression
Y <- -2 + 3.5 * X + u
# population <- data.frame(X, Y)
population <- tibble(X, Y)


# set sample size
n <- 100

# compute the variance of beta_hat_0
H_i <- 1 - mean(X) / mean(X^2) * X
var_b0 <- var(H_i * u) / (n * mean(H_i^2)^2 )

# compute the variance of hat_beta_1
var_b1 <- var( ( X - mean(X) ) * u ) / (n * var(X)^2)


# print variances to the console
var_b0
#> [1] 4.045066
var_b1
#> [1] 0.03018694


# set repetitions and sample size
n <- 100
reps <- 10000

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# loop sampling and estimation of the coefficients
for (i in 1:reps){
    
    sample <- population[sample(1:N, n), ]
    fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
    
}

# compute variance estimates using outcomes
var(fit[, 1])
#> [1] 4.186832
var(fit[, 2])
#> [1] 0.03096199


# divide plotting area as 1-by-2 array
# par(mfrow = c(1, 2))

# plot histograms of beta_0 estimates
# hist(fit[, 1],
#      cex.main = 0.8,
#      main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[0] ~ Estimates), 
#      xlab = bquote(hat(beta)[0]), 
#      freq = F)

# add true distribution to plot
# curve(dnorm(x, 
#             -2, 
#             sqrt(var_b0)), 
#       add = T, 
#       col = "darkred",lwd=2)

# plot histograms of beta_hat_1 
# hist(fit[, 2],
#      cex.main = 0.8,
#      main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[1] ~ Estimates), 
#      xlab = bquote(hat(beta)[1]), 
#      freq = F)

# add true distribution to plot
# curve(dnorm(x, 
#             3.5, 
#             sqrt(var_b1)), 
#       add = T, 
#       col = "darkred",lwd=2)
p1 <- ggplot() +
  geom_histogram(aes(x = fit[, 1], y = after_stat(density)), 
                 bins = 30, fill = "steelblue", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = -2, sd = sqrt(var_b0)), 
                color = "darkred", linewidth = 1) +
  labs(x = expression(hat(beta)[0]), y = "Density", 
       title = expression(paste("The Distribution of 10000 ", beta[0], " Estimates")))

p2 <- ggplot() +
  geom_histogram(aes(x = fit[, 2], y = after_stat(density)), 
                 bins = 30, fill = "steelblue", color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = 3.5, sd = sqrt(var_b1)), 
                color = "darkred", linewidth = 1) +
  labs(x = expression(hat(beta)[1]), y = "Density", 
       title = expression(paste("The Distribution of 10000 ", beta[1], " Estimates")))

p1 + p2


# set seed for reproducibility
set.seed(1)

# set repetitions and the vector of sample sizes
reps <- 1000
n <- c(100, 250, 1000, 3000)

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# divide the plot panel in a 2-by-2 array
# par(mfrow = c(2, 2))

# loop sampling and plotting

# outer loop over n
# for (j in 1:length(n)) {
# 
#     # inner loop: sampling and estimating of the coefficients
#     for (i in 1:reps){
# 
#         sample <- population[sample(1:N, n[j]), ]
#         fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
# 
#     }
#     print(fit)
#     # draw density estimates
#     plot(density(fit[ ,2]), xlim=c(2.5, 4.5),
#          col = j,
#          main = paste("n=", n[j]),
#          xlab = bquote(hat(beta)[1]))
# 
# }
p <- list()
# outer loop over n
for (j in 1:length(n)) {

    # inner loop: sampling and estimating of the coefficients
    for (i in 1:reps){

        sample <- population[sample(1:N, n[j]), ]
        fit[i, ] <- lm(Y ~ X, data = sample)$coefficients

    }
    
    df <- tibble(fit = fit[, 2])
    
    # draw density estimates
    p[[j]] <- ggplot() +
      geom_density(aes(x = fit), data = df, color = j) +
      labs(x = expression(hat(beta)[1]), y = "Density", 
           title = paste("n=", n[j])) +
      xlim(c(2.5, 4.5))
}

# p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)
p[[1]] + p[[2]] + p[[3]] + p[[4]] + plot_layout(ncol = 2, nrow = 2)


# set seed for reproducibility
set.seed(4)

# simulate bivariate normal data
bvndata <- mvrnorm(100, 
                   mu = c(5, 5), 
                   Sigma = cbind(c(5, 4), c(4, 5))) 

# assign column names / convert to data.frame
colnames(bvndata) <- c("X", "Y")
# bvndata <- as.data.frame(bvndata)
bvndata <- as_tibble(bvndata)

# subset the data
# set1 <- subset(bvndata, abs(mean(X) - X) > 1)
# set2 <- subset(bvndata, abs(mean(X) - X) <= 1)
set1 <- bvndata %>% 
  filter(abs(mean(X) - X) > 1)
set2 <- bvndata %>%
  filter(abs(mean(X) - X) <= 1)

# plot both data sets
# plot(set1, 
#      xlab = "X", 
#      ylab = "Y", 
#      pch = 19)
# 
# points(set2, 
#        col = "steelblue", 
#        pch = 19)
# legend("topleft", 
#        legend = c("Set1", 
#                   "Set2"),
#        cex = 1,
#        pch = 19,
#        col = c("black","steelblue"))
ggplot() +
  geom_point(aes(x = X, y = Y, color = "Set1"), data = set1) +
  geom_point(aes(x = X, y = Y, color = "Set2"), data = set2) +
  scale_color_manual(name = NULL, 
                     values = c("Set1" = "black", 
                                "Set2" = "steelblue")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) 


# estimate both regression lines
lm.set1 <- lm(Y ~ X, data = set1)
lm.set2 <- lm(Y ~ X, data = set2)

# plot observations
# plot(set1, xlab = "X", ylab = "Y", pch = 19)
# points(set2, col = "steelblue", pch = 19)

# add both lines to the plot
# abline(lm.set1, col = "black",lwd=2)
# abline(lm.set2, col = "steelblue",lwd=2)
# legend("bottomright", 
#        legend = c("Set1", 
#                   "Set2"),
#        cex = 1,
#        lwd=2,
#        col = c("black","steelblue"))
ggplot() +
  geom_point(aes(x = X, y = Y, color = "Set1"), data = set1) +
  geom_point(aes(x = X, y = Y, color = "Set2"), data = set2) +
  geom_abline(aes(intercept = lm.set1$coefficients[1], 
                  slope = lm.set1$coefficients[2], 
                  color = "Set1")) +
  geom_abline(aes(intercept = lm.set2$coefficients[1], 
                  slope = lm.set2$coefficients[2], 
                  color = "Set2")) +
  scale_color_manual(name = NULL, 
                     values = c("Set1" = "black", 
                                "Set2" = "steelblue")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))


# 4.6 Exercises ----------------------------------------------------------------


# 1. Class Sizes and Test Scores
# create both vectors
cs <- c(23, 19, 30, 22, 23, 29, 35, 36, 33, 25)
ts <- c(430, 430, 333, 410, 390, 377, 325, 310, 328, 375)


# draw the scatterplot
# plot(ts ~ cs,
#      xlab = "Class Size",
#      ylab = "Test Score",
#      main = "Scatterplot of Test Score and Class Size",
#      pch = 20)
ggplot() +
  geom_point(aes(x = cs, y = ts)) +
  labs(x = "Class Size", y = "Test Score", 
       title = "Scatterplot of Test Score and Class Size")


# 2. Mean, Variance, Covariance and Correlation
# compute mean, variance and standard deviation of test scores
mean(ts)
#> [1] 370.8
sd(ts)
#> [1] 44.75315
var(ts)
#> [1] 2002.844

# compute the covariance and the correlation coefficient
cov(cs, ts)
#> [1] -251.4444
cor(cs, ts)
#> [1] -0.9474424


# 3. Simple Linear Regression
# attach the package AER
# library(AER)

# estimate the model
mod <- lm(ts ~ cs)
mod
# Call:
# lm(formula = ts ~ cs)
# 
# Coefficients:
# (Intercept)           cs  
#      567.43        -7.15 

# obtain a model summary
summary(mod)
# Call:
# lm(formula = ts ~ cs)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -19.9248 -10.6002  -0.8506   5.8631  27.0246 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 567.4272    23.9606  23.682 1.08e-08 ***
# cs           -7.1501     0.8536  -8.376 3.13e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.19 on 8 degrees of freedom
# Multiple R-squared:  0.8976,	Adjusted R-squared:  0.8849 
# F-statistic: 70.16 on 1 and 8 DF,  p-value: 3.132e-05


# 4. The Model Object
# check the class of `mod`
class(mod)
#> [1] "lm"

# use `is.list()` on `mod`
is.list(mod)
#> [1] TRUE

# check the entries of `mod` using `names()`
names(mod)
#>  [1] "coefficients"  "residuals"     "effects"       "rank"
#>  [5] "fitted.values" "assign"        "qr"            "df.residual"
#>  [9] "xlevels"       "call"          "terms"         "model"

# use the operator `$` on `mod`
mod$coefficients
#> (Intercept)          cs
#>    567.4272     -7.1501


# 5. Plotting the Regression Line
# add the regression line to the scatterplot
# plot(cs, ts)
# abline(mod)
ggplot() +
  geom_point(aes(x = cs, y = ts)) +
  geom_abline(aes(intercept = mod$coefficients[1], 
                  slope = mod$coefficients[2])) +
  labs(x = "Class Size", y = "Test Score", 
       title = "Scatterplot of Test Score and Class Size")


# 6. Summary of a Model Object
# assign the model summary to the variable `s`
s <- summary(mod)

# check names of entries in `s`
names(s)
#>  [1] "call"          "terms"         "residuals"     "coefficients"
#>  [5] "aliased"       "sigma"         "df"            "r.squared"
#>  [9] "adj.r.squared" "fstatistic"    "cov.unscaled"

# save the R^2 of the regression to the variable `R2`
R2 <- s$r.squared
R2
#> [1] 0.8976472


# 7. Estimated Coefficients
# save the coefficient matrix to `coefs`
coefs <- s$coefficients
coefs
#>               Estimate Std. Error   t value     Pr(>|t|)
#> (Intercept) 567.427172 23.9606448 23.681632 1.075914e-08
#> cs           -7.150079  0.8536168 -8.376216 3.132287e-05


# 8. Dropping the Intercept
# regress `ts` solely on `cs`. Store the result in `mod_ni`.
mod_ni <- lm(ts ~ cs - 1)
mod_ni
# Call:
# lm(formula = ts ~ cs - 1)
# 
# Coefficients:
#    cs  
# 12.65 


# 9. Regression Output: No Constant Case
# extract the coefficient matrix from the model summary and save it to `coef`
coef <- summary(mod_ni)$coefficients



# 10. Regression Output: No Constant Case - Ctd.
# print the contents of `coef` to the console
coef
#>    Estimate Std. Error  t value    Pr(>|t|)
#> cs 12.65478    1.36013 9.304097 6.50056e-06

# compute the t-statistic manually and assign it to `t_stat`
t_stat <- coef[1, 1] / coef[1, 2]
t_stat
#> [1] 9.304097


# 11. Two Regressions, One Plot
# plot the regression lines of both models
# plot(cs, ts)
# abline(mod, col = "blue")
# abline(mod_ni, col = "red")
ggplot() +
  geom_point(aes(x = cs, y = ts)) +
  geom_abline(aes(intercept = mod$coefficients[1], 
                  slope = mod$coefficients[2],
                  color = "Intercept")) +
  geom_abline(aes(intercept = 0, 
                  slope = mod_ni$coefficients[1], 
                  color = "No Intercept")) + 
  scale_color_manual(name = NULL, 
                     values = c("Intercept" = "blue", 
                                "No Intercept" = "red")) +
  labs(x = "Class Size", y = "Test Score", 
       title = "Scatterplot of Test Score and Class Size")


# 12. TSS and SSR
# compute the SSR and save it to `ssr`
ssr <- sum(mod$residuals^2)
ssr
#> [1] 1844.971

# compute the TSS and save it to `tss`
tss <- var(ts) * (length(ts) - 1)
tss
#> [1] 18025.6


# 13. The R^2 of a Regression Model
# compute R^2, round to four decimal places and save the result to `R2`
R2 <- round(1 - ssr / tss, 4)
R2
#> [1] 0.8976

# check whether your result is correct using the "==" operator
R2 == round(summary(mod)$r.squared, 4)
#> [1] TRUE


# 14. The Standard Error of The Regression
# obtain the SER using `summary()` and save the value to `SER`
SER <- summary(mod)$sigma
SER
#> [1] 15.18622

# compute the SSR and save it to `SSR`
SSR <- SER^2 * (length(ts) - 2)
SSR
#> [1] 1844.971

# do the comparison
SER == sqrt(SSR / (length(ts) - 2))
#> [1] TRUE