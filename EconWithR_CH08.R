# Econometrics with R
# Chapter 08 Nonlinear Regression Functions ------------------------------------
library(AER)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(patchwork)


# 8.1 A General Strategy for Modelling Nonlinear Regression Functions ----------

# prepare the data
# library(AER)                                                     
data(CASchools)
# CASchools$size <- CASchools$students/CASchools$teachers
# CASchools$score <- (CASchools$read + CASchools$math) / 2       
CASchools <- CASchools %>%
  mutate(size = students/teachers,
         score = (read + math) / 2)


cor(CASchools$income, CASchools$score)
#> [1] 0.7124308


# fit a simple linear model
linear_model<- lm(score ~ income, data = CASchools)

# plot the observations
# plot(CASchools$income, CASchools$score,
#      col = "steelblue",
#      pch = 20,
#      xlab = "District Income (thousands of dollars)", 
#      ylab = "Test Score",
#      cex.main = 0.9,
#      main = "Test Score vs. District Income and a Linear OLS Regression Function")

# add the regression line to the plot
# abline(linear_model, 
#        col = "red", 
#        lwd = 2)
# legend("bottomright", legend="linear fit",lwd=2,col="red")
ggplot(CASchools, aes(x = income, y = score)) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(color = "linear fit"), formula = y ~ x,
              method = "lm", se = FALSE) +
  scale_color_manual(name = NULL, values = c("linear fit" = "red")) +
  labs(x = "District Income (thousands of dollars)", y = "Test Score",
       title = "Test Score vs. District Income and a Linear OLS Regression Function")+
  theme(legend.position = c(0.95, 0.05), legend.justification = c(0.8, 0.2))


# fit the quadratic Model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)

# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>                Estimate  Std. Error  t value  Pr(>|t|)    
#> (Intercept) 607.3017435   2.9017544 209.2878 < 2.2e-16 ***
#> income        3.8509939   0.2680942  14.3643 < 2.2e-16 ***
#> I(income^2)  -0.0423084   0.0047803  -8.8505 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# draw a scatterplot of the observations for income and test score
# plot(CASchools$income, CASchools$score,
#      col  = "steelblue",
#      pch = 20,
#      xlab = "District Income (thousands of dollars)",
#      ylab = "Test Score",
#      main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
# abline(linear_model, col = "green", lwd = 2)

# add quatratic function to the plot
# order_id <- order(CASchools$income)
# 
# lines(x = CASchools$income[order_id], 
#       y = fitted(quadratic_model)[order_id],
#       col = "red", 
#       lwd = 2) 
# legend("bottomright",legend=c("Linear Line","Quadratic Line"),
#        lwd=2,col=c("green","red"))
ggplot(CASchools, aes(x = income, y = score)) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(color = "Linear Line"), method = "lm", 
              formula = y ~ x, se = FALSE) +
  geom_smooth(aes(color = "Quadratic Line"), method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE), se = FALSE) +
  scale_color_manual(name = NULL, values = c("Linear Line" = "green",
                                             "Quadratic Line" = "red")) +
  labs(x = "District Income (thousands of dollars)", y = "Test Score",
       title = "Estimated Linear and Quadratic Regression Functions")+
  theme(legend.position = c(0.95, 0.05), legend.justification = c(0.8, 0.2))


# 8.2 Nonlinear Functions of a Single Independent Variable ---------------------

# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)


# test the hypothesis of a linear model against quadratic or polynomial
# alternatives

# set up hypothesis matrix
R <- rbind(c(0, 0, 1, 0),
           c(0, 0, 0, 1))

# do the test
linearHypothesis(cubic_model,
                 hypothesis.matrix = R,
                 white.adj = "hc1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> poly(income, degree = 3, raw = TRUE)2 = 0
#> poly(income, degree = 3, raw = TRUE)3 = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ poly(income, degree = 3, raw = TRUE)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1    418                        
#> 2    416  2 37.691 9.043e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


summary(cubic_model)
#> 
#> Call:
#> lm(formula = score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -44.28  -9.21   0.20   8.32  31.16 
#> 
#> Coefficients:
#>                                         Estimate Std. Error t value Pr(>|t|)
#> (Intercept)                            6.001e+02  5.830e+00 102.937  < 2e-16
#> poly(income, degree = 3, raw = TRUE)1  5.019e+00  8.595e-01   5.839 1.06e-08
#> poly(income, degree = 3, raw = TRUE)2 -9.581e-02  3.736e-02  -2.564   0.0107
#> poly(income, degree = 3, raw = TRUE)3  6.855e-04  4.720e-04   1.452   0.1471
#>                                          
#> (Intercept)                           ***
#> poly(income, degree = 3, raw = TRUE)1 ***
#> poly(income, degree = 3, raw = TRUE)2 *  
#> poly(income, degree = 3, raw = TRUE)3    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 12.71 on 416 degrees of freedom
#> Multiple R-squared:  0.5584, Adjusted R-squared:  0.5552 
#> F-statistic: 175.4 on 3 and 416 DF,  p-value: < 2.2e-16


# test the hypothesis using robust standard errors
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>                                          Estimate  Std. Error  t value
#> (Intercept)                            6.0008e+02  5.1021e+00 117.6150
#> poly(income, degree = 3, raw = TRUE)1  5.0187e+00  7.0735e-01   7.0950
#> poly(income, degree = 3, raw = TRUE)2 -9.5805e-02  2.8954e-02  -3.3089
#> poly(income, degree = 3, raw = TRUE)3  6.8549e-04  3.4706e-04   1.9751
#>                                        Pr(>|t|)    
#> (Intercept)                           < 2.2e-16 ***
#> poly(income, degree = 3, raw = TRUE)1 5.606e-12 ***
#> poly(income, degree = 3, raw = TRUE)2  0.001018 ** 
#> poly(income, degree = 3, raw = TRUE)3  0.048918 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# perform robust F-test 
linearHypothesis(cubic_model, 
                 hypothesis.matrix = R,
                 vcov. = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> poly(income, degree = 3, raw = TRUE)2 = 0
#> poly(income, degree = 3, raw = TRUE)3 = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ poly(income, degree = 3, raw = TRUE)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1    418                        
#> 2    416  2 29.678 8.945e-13 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute and assign the quadratic model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)

# set up data for prediction
# new_data <- data.frame(income = c(10, 11))
new_data <- tibble(income = c(10, 11))

# do the prediction
Y_hat <- predict(quadratic_model, newdata = new_data)

# compute the difference
diff(Y_hat)
#>        2 
#> 2.962517


# set up data for prediction
# new_data <- data.frame(income = c(40, 41))
new_data <- tibble(income = c(40, 41))

# do the prediction
Y_hat <- predict(quadratic_model, newdata = new_data)

# compute the difference
diff(Y_hat)
#>         2 
#> 0.4240097


# estimate a level-log model
LinearLog_model <- lm(score ~ log(income), data = CASchools)

# compute robust summary
coeftest(LinearLog_model, 
         vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 557.8323     3.8399 145.271 < 2.2e-16 ***
#> log(income)  36.4197     1.3969  26.071 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# draw a scatterplot
# plot(score ~ income, 
#      col = "steelblue",
#      pch = 20,
#      data = CASchools,
#      ylab="Score",
#      xlab="Income",
#      main = "Linear-Log Regression Line")

# add the linear-log regression line
# order_id  <- order(CASchools$income)
# 
# lines(CASchools$income[order_id],
#       fitted(LinearLog_model)[order_id], 
#       col = "red", 
#       lwd = 2)
# legend("bottomright",legend = "Linear-log line",lwd = 2,col ="red")
ggplot(CASchools, aes(x = income, y = score)) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(color = "Linear-log line"), method = "lm", 
              formula = y ~ log(x), se = FALSE) +
  scale_color_manual(name = NULL, values = c("Linear-log line" = "red")) +
  labs(x = "District Income (thousands of dollars)", y = "Test Score",
       title = "Linear-Log Regression Line")+
  theme(legend.position = c(0.95, 0.05), legend.justification = c(0.8, 0.2))


# set up new data
# new_data <- data.frame(income = c(10, 11, 40, 41))
new_data <- tibble(income = c(10, 11, 40, 41))

# predict the outcomes 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# compute the expected difference
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE)
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]
#> [1] 3.471166 0.899297


# estimate a log-linear model 
LogLinear_model <- lm(log(score) ~ income, data = CASchools)

# obtain a robust coefficient summary
coeftest(LogLinear_model, 
         vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 6.43936234 0.00289382 2225.210 < 2.2e-16 ***
#> income      0.00284407 0.00017509   16.244 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# estimate the log-log model
LogLog_model <- lm(log(score) ~ log(income), data = CASchools)

# print robust coefficient summary to the console
coeftest(LogLog_model, 
         vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 6.3363494  0.0059246 1069.501 < 2.2e-16 ***
#> log(income) 0.0554190  0.0021446   25.841 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# generate a scatterplot
# plot(log(score) ~ income, 
#      col = "steelblue", 
#      pch = 20, 
#      data = CASchools,
#      ylab="log(Score)",
#      xlab="Income",
#      main = "Log-Linear Regression Function")

# add the log-linear regression line
# order_id  <- order(CASchools$income)

# lines(CASchools$income[order_id], 
#       fitted(LogLinear_model)[order_id], 
#       col = "red", 
#       lwd = 2)

# add the log-log regression line
# lines(sort(CASchools$income), 
#       fitted(LogLog_model)[order(CASchools$income)], 
#       col = "green", 
#       lwd = 2)

# add a legend
# legend("bottomright",
#        legend = c("log-linear model", "log-log model"),
#        lwd = 2,
#        col = c("red", "green"))
ggplot(CASchools, aes(x = income, y = log(score))) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(color = "log-linear model"), method = "lm", 
              formula = y ~ x, se = FALSE) +
  geom_smooth(aes(color = "log-log model"), method = "lm", 
              formula = y ~ log(x), se = FALSE) +
  scale_color_manual(name = NULL, values = c("log-linear model" = "red",
                                             "log-log model" = "green")) +
  labs(x = "District Income (thousands of dollars)", y = "log(Test Score)",
       title = "Log-Linear Regression Function")+
  theme(legend.position = c(0.95, 0.05), legend.justification = c(0.8, 0.2))


# estimate the polylog model
polyLog_model <- lm(score ~ log(income) + I(log(income)^2) + I(log(income)^3), 
                    data = CASchools)

# print robust summary to the console
coeftest(polyLog_model, 
         vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>                  Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)      486.1341    79.3825  6.1239 2.115e-09 ***
#> log(income)      113.3820    87.8837  1.2901    0.1977    
#> I(log(income)^2) -26.9111    31.7457 -0.8477    0.3971    
#> I(log(income)^3)   3.0632     3.7369  0.8197    0.4128    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute the adj. R^2 for the nonlinear models
adj_R2 <-rbind("quadratic" = summary(quadratic_model)$adj.r.squared,
               "cubic" = summary(cubic_model)$adj.r.squared,
               "LinearLog" = summary(LinearLog_model)$adj.r.squared,
               "LogLinear" = summary(LogLinear_model)$adj.r.squared,
               "LogLog" = summary(LogLog_model)$adj.r.squared,
               "polyLog" = summary(polyLog_model)$adj.r.squared)

# assign column names
colnames(adj_R2) <- "adj_R2"

adj_R2
#>              adj_R2
#> quadratic 0.5540444
#> cubic     0.5552279
#> LinearLog 0.5614605
#> LogLinear 0.4970106
#> LogLog    0.5567251
#> polyLog   0.5599944


# generate a scatterplot
# plot(score ~ income, 
#      data = CASchools,
#      col = "steelblue", 
#      pch = 20,
#      ylab="Score",
#      xlab="Income",
#      main = "Linear-Log and Cubic Regression Functions")

# add the linear-log regression line
# order_id  <- order(CASchools$income)

# lines(CASchools$income[order_id],
#       fitted(LinearLog_model)[order_id], 
#       col = "darkgreen", 
#       lwd = 2)

# add the cubic regression line
# lines(x = CASchools$income[order_id], 
#       y = fitted(cubic_model)[order_id],
#       col = "red", 
#       lwd = 2)
# add a legend
# legend("bottomright",
#        legend = c("Linear-Log model", "Cubic model"),
#        lwd = 2,
#        col = c("darkgreen", "red"))
ggplot(CASchools, aes(x = income, y = score)) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(color = "Linear-Log model"), method = "lm", 
              formula = y ~ log(x), se = FALSE) +
  geom_smooth(aes(color = "Cubic model"), method = "lm", 
              formula = y ~ poly(x, 3, raw = TRUE), se = FALSE) +
  scale_color_manual(name = NULL, values = c("Linear-Log model" = "darkgreen",
                                             "Cubic model" = "red")) +
  labs(x = "District Income (thousands of dollars)", y = "Test Score",
       title = "Linear-Log and Cubic Regression Functions")+
  theme(legend.position = c(0.95, 0.05), legend.justification = c(0.8, 0.2))


# 8.3 Interactions Between Independent Variables -------------------------------

# append HiSTR to CASchools
# CASchools$HiSTR <- as.numeric(CASchools$size >= 20)

# append HiEL to CASchools
# CASchools$HiEL <- as.numeric(CASchools$english >= 10)

CASchools <- CASchools %>%
  mutate(HiSTR = as.numeric(size >= 20),
         HiEL = as.numeric(english >= 10))


# estimate the model with a binary interaction term
bi_model <- lm(score ~ HiSTR * HiEL, data = CASchools)

# print a robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 664.1433     1.3881 478.4589 < 2.2e-16 ***
#> HiSTR        -1.9078     1.9322  -0.9874    0.3240    
#> HiEL        -18.3155     2.3340  -7.8472 3.634e-14 ***
#> HiSTR:HiEL   -3.2601     3.1189  -1.0453    0.2965    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# estimate means for all combinations of HiSTR and HiEL

# 1.
# predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 0))
predict(bi_model, newdata = tibble("HiSTR" = 0, "HiEL" = 0))
#>        1 
#> 664.1433

# 2.
# predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 1))
predict(bi_model, newdata = tibble("HiSTR" = 0, "HiEL" = 1))
#>        1 
#> 645.8278

# 3.
# predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 0))
predict(bi_model, newdata = tibble("HiSTR" = 1, "HiEL" = 0))
#>        1 
#> 662.2354

# 4.
# predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 1))
predict(bi_model, newdata = tibble("HiSTR" = 1, "HiEL" = 1))
#>        1 
#> 640.6598


# generate artificial data
set.seed(1)

X <- runif(200,0, 15)
D <- sample(0:1, 200, replace = T)
Y <- 450 +  150 * X + 500 * D + 50 * (X * D) + rnorm(200, sd = 300)

# divide plotting area accordingly
# m <- rbind(c(1, 2), c(3, 0))
# graphics::layout(m)

# estimate the models and plot the regression lines
# 1. (baseline model)
# plot(X, log(Y),
#      pch = 20,
#      col = "steelblue",
#      main = "Different Intercepts, Same Slope",
#      cex.main=1.2)
# 
mod1_coef <- lm(log(Y) ~ X + D)$coefficients
# 
# abline(coef = c(mod1_coef[1], mod1_coef[2]), 
#        col = "red",
#        lwd = 1.5)
# 
# abline(coef = c(mod1_coef[1] + mod1_coef[3], mod1_coef[2]), 
#        col = "green",
#        lwd = 1.5)
p1 <- ggplot(tibble(X, Y, D), aes(x = X, y = log(Y))) +
    geom_point(col = "steelblue") +
    geom_abline(aes(intercept = mod1_coef[1], 
                    slope = mod1_coef[2]), 
                color = "red") +
    geom_abline(aes(intercept = mod1_coef[1] + mod1_coef[3], 
                    slope = mod1_coef[2]),
                color = "green") +
    labs(x = "X", y = "log(Y)",
         title = "Different Intercepts, Same Slope")  

# 2. (baseline model + interaction term)
# plot(X, log(Y),
#      pch = 20,
#      col = "steelblue",
#      main = "Different Intercepts, Different Slopes",
#      cex.main=1.2)
# 
mod2_coef <- lm(log(Y) ~ X + D + X:D)$coefficients
# 
# abline(coef = c(mod2_coef[1], mod2_coef[2]), 
#        col = "red",
#        lwd = 1.5)
# 
# abline(coef = c(mod2_coef[1] + mod2_coef[3], mod2_coef[2] + mod2_coef[4]), 
#        col = "green",
#        lwd = 1.5)
p2 <- ggplot(tibble(X, Y, D), aes(x = X, y = log(Y))) +
    geom_point(col = "steelblue") +
    geom_abline(aes(intercept = mod2_coef[1], 
                    slope = mod2_coef[2]), 
                color = "red") +
    geom_abline(aes(intercept = mod2_coef[1] + mod2_coef[3], 
                    slope = mod2_coef[2] + mod2_coef[4]),
                color = "green") +
    labs(x = "X", y = "log(Y)",
         title = "Different Intercepts, Different Slopes")

# 3. (omission of D as regressor + interaction term)
# plot(X, log(Y),
#      pch = 20,
#      col = "steelblue",
#      main = "Same Intercept, Different Slopes",
#      cex.main=1.2)
# 
mod3_coef <- lm(log(Y) ~ X + X:D)$coefficients
# 
# abline(coef = c(mod3_coef[1], mod3_coef[2]), 
#        col = "red",
#        lwd = 2)
# 
# abline(coef = c(mod3_coef[1], mod3_coef[2] + mod3_coef[3]), 
#        col = "green",
#        lwd = 2)
p3 <- ggplot(tibble(X, Y, D), aes(x = X, y = log(Y))) +
    geom_point(col = "steelblue") +
    geom_abline(aes(intercept = mod3_coef[1], 
                    slope = mod3_coef[2]), 
                color = "red") +
    geom_abline(aes(intercept = mod3_coef[1], 
                    slope = mod3_coef[2] + mod3_coef[3]),
                color = "green") +
    labs(x = "X", y = "log(Y)",
         title = "Same Intercept, Different Slopes")

p1 + p2 + p3 + plot_layout(ncol = 2, nrow = 2)


# estimate the model
bci_model <- lm(score ~ size + HiEL + size * HiEL, data = CASchools)

# print robust summary of coefficients to the console
coeftest(bci_model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 682.24584   11.86781 57.4871   <2e-16 ***
#> size         -0.96846    0.58910 -1.6440   0.1009    
#> HiEL          5.63914   19.51456  0.2890   0.7727    
#> size:HiEL    -1.27661    0.96692 -1.3203   0.1875    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# identify observations with PctEL >= 10
# id <- CASchools$english >= 10

# plot observations with HiEL = 0 as red dots
# plot(CASchools$size[!id], CASchools$score[!id],
#      xlim = c(0, 27),
#      ylim = c(600, 720),
#      pch = 20,
#      col = "red",
#      main = "",
#      xlab = "Class Size",
#      ylab = "Test Score")

# plot observations with HiEL = 1 as green dots
# points(CASchools$size[id], CASchools$score[id],
#        pch = 20,
#        col = "green")

# read out estimated coefficients of bci_model
coefs <- bci_model$coefficients

# draw the estimated regression line for HiEL = 0
# abline(coef = c(coefs[1], coefs[2]),
#        col = "red",
#        lwd = 1.5)

# draw the estimated regression line for HiEL = 1
# abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]),
#        col = "green", 
#        lwd = 1.5 )

# add a legend to the plot
# legend("topleft", 
#        pch = c(20, 20), 
#        col = c("red", "green"), 
#        legend = c("HiEL = 0", "HiEL = 1"))
ggplot(CASchools, aes(x = size, y = score)) +
  geom_point(aes(color = "HiEL = 0"), data = filter(CASchools, HiEL == 0)) +
  geom_point(aes(color = "HiEL = 1"), data = filter(CASchools, HiEL == 1)) +
  geom_abline(aes(intercept = coefs[1], slope = coefs[2]), 
              color = "red") +
  geom_abline(aes(intercept = coefs[1] + coefs[3], 
                  slope = coefs[2] + coefs[4]),
              color = "green") +
  scale_color_manual(name = NULL, values = c("HiEL = 0" = "red",
                                             "HiEL = 1" = "green")) +
  labs(x = "Class Size", y = "Test Score",
       title = "Estimated Regression Lines for HiEL = 0 and HiEL = 1")+
  xlim(0, 27) +
  ylim(600, 720) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# estimate regression model including the interaction between 'PctEL' and 'size'
cci_model <- lm(score ~ size + english + english * size, data = CASchools) 

# print a summary to the console
coeftest(cci_model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>                 Estimate  Std. Error t value Pr(>|t|)    
#> (Intercept)  686.3385268  11.7593466 58.3654  < 2e-16 ***
#> size          -1.1170184   0.5875136 -1.9013  0.05796 .  
#> english       -0.6729119   0.3741231 -1.7986  0.07280 .  
#> size:english   0.0011618   0.0185357  0.0627  0.95005    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


summary(CASchools$english)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.000   1.941   8.778  15.768  22.970  85.540


data("Journals")
Journals <- as_tibble(Journals)


# define and rename variables
# Journals$PricePerCitation <- Journals$price/Journals$citations
# Journals$Age <- 2000 - Journals$foundingyear
# Journals$Characters <- Journals$charpp * Journals$pages/10^6
# Journals$Subscriptions <- Journals$subs
Journals <- Journals %>%
  mutate(PricePerCitation = price/citations,
         Age = 2000 - foundingyear,
         Characters = charpp * pages/10^6,
         Subscriptions = subs)


# compute summary statistics for price per citation
summary(Journals$PricePerCitation)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#>  0.005223  0.464495  1.320513  2.548455  3.440171 24.459459


# Estimate models (I) - (IV)
J_mod1 <- lm(log(Subscriptions) ~ log(PricePerCitation), 
             data = Journals)

J_mod2 <- lm(log(Subscriptions) ~ log(PricePerCitation) 
             + log(Age) + log(Characters), 
             data = Journals)

J_mod3 <- lm(log(Subscriptions) ~ 
                 log(PricePerCitation) + I(log(PricePerCitation)^2) 
             + I(log(PricePerCitation)^3) + log(Age) 
             + log(Age):log(PricePerCitation) + log(Characters), 
             data = Journals)

J_mod4 <- lm(log(Subscriptions) ~ 
                 log(PricePerCitation) + log(Age) 
             + log(Age):log(PricePerCitation) + 
                 log(Characters), 
             data = Journals)


# F-Test for significance of cubic terms
linearHypothesis(J_mod3, 
                 c("I(log(PricePerCitation)^2)=0", "I(log(PricePerCitation)^3)=0"),
                 vcov. = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(log(PricePerCitation)^2) = 0
#> I(log(PricePerCitation)^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: log(Subscriptions) ~ log(PricePerCitation) + I(log(PricePerCitation)^2) + 
#>     I(log(PricePerCitation)^3) + log(Age) + log(Age):log(PricePerCitation) + 
#>     log(Characters)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    175                 
#> 2    173  2 0.1943 0.8236


rob_se <- list(sqrt(diag(vcovHC(J_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(J_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(J_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(J_mod4, type = "HC1"))))

# generate a Latex table using stargazer
stargazer(J_mod1, J_mod2, J_mod3, J_mod4,
          se = rob_se,
          digits = 3,
          column.labels = c("(I)", "(II)", "(III)", "(IV)"),
          type = "html", out = "output/table_CH08_1.html")


# divide plotting area
# m <- rbind(c(1, 2), c(3, 0))
# graphics::layout(m)

# scatterplot
# plot(Journals$PricePerCitation, 
#      Journals$Subscriptions, 
#      pch = 20, 
#      col = "steelblue",
#      ylab = "Subscriptions",
#      xlab = "Price per citation",
#      main = "(a)")
p1 <- ggplot(Journals, aes(x = PricePerCitation, y = Subscriptions)) +
    geom_point(col = "steelblue") +
    labs(x = "Price per citation", y = "Subscriptions", title = "(a)")

# log-log scatterplot and estimated regression line (I)
# plot(log(Journals$PricePerCitation), 
#      log(Journals$Subscriptions), 
#      pch = 20, 
#      col = "steelblue",
#      ylab = "ln(Subscriptions)",
#      xlab = "ln(Price per citation)",
#      main = "(b)")

# abline(J_mod1,
#        lwd = 1.5)
p2 <- ggplot(Journals, aes(x = log(PricePerCitation), y = log(Subscriptions))) +
    geom_point(col = "steelblue") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "ln(Price per citation)", y = "ln(Subscriptions)", title = "(b)")

# log-log scatterplot and regression lines (IV) for Age = 5 and Age = 80
# plot(log(Journals$PricePerCitation), 
#      log(Journals$Subscriptions), 
#      pch = 20, 
#      col = "steelblue",
#      ylab = "ln(Subscriptions)",
#      xlab = "ln(Price per citation)",
#      main = "(c)")

JM4C <-J_mod4$coefficients

# Age = 80
# abline(coef = c(JM4C[1] + JM4C[3] * log(80), 
#                 JM4C[2] + JM4C[5] * log(80)),
#        col = "darkred",
#        lwd = 1.5)

# Age = 5
# abline(coef = c(JM4C[1] + JM4C[3] * log(5), 
#                 JM4C[2] + JM4C[5] * log(5)),
#        col = "darkgreen",
#        lwd = 1.5)
p3 <- ggplot(Journals, aes(x = log(PricePerCitation), y = log(Subscriptions))) +
    geom_point(col = "steelblue") +
    geom_abline(aes(intercept = JM4C[1] + JM4C[3] * log(80), 
                    slope = JM4C[2] + JM4C[5] * log(80)),
                color = "darkred") +
    geom_abline(aes(intercept = JM4C[1] + JM4C[3] * log(5), 
                    slope = JM4C[2] + JM4C[5] * log(5)),
                color = "darkgreen") +
    labs(x = "ln(Price per citation)", y = "ln(Subscriptions)", title = "(c)")

p1 + p2 + p3 + plot_layout(ncol = 2, nrow = 2)


# 8.4 Nonlinear Effects on Test Scores of the Student-Teacher Ratio ------------

# estimate all models
TS_mod1 <- lm(score ~ size + english + lunch, data = CASchools)

TS_mod2 <- lm(score ~ size + english + lunch + log(income), data = CASchools)

TS_mod3 <- lm(score ~ size + HiEL + HiEL:size, data = CASchools)

TS_mod4 <- lm(score ~ size + HiEL + HiEL:size + lunch + log(income), data = CASchools)

TS_mod5 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + lunch + log(income),
              data = CASchools)

TS_mod6 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + HiEL:size + HiEL:I(size^2) +
                  HiEL:I(size^3) + lunch + log(income), data = CASchools)

TS_mod7 <- lm(score ~ size + I(size^2) + I(size^3) + english + lunch + log(income),
              data = CASchools)


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(TS_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(TS_mod7, type = "HC1"))))

# generate a LaTeX table of regression outputs
stargazer(TS_mod1, 
          TS_mod2, 
          TS_mod3, 
          TS_mod4, 
          TS_mod5, 
          TS_mod6, 
          TS_mod7,
          digits = 3,
          dep.var.caption = "Dependent Variable: Test Score",
          se = rob_se,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"),
          type = "html", out = "output/table_CH08_2.html")


# check joint significance of the interaction terms
linearHypothesis(TS_mod6, 
                 c("size:HiEL=0", "I(size^2):HiEL=0", "I(size^3):HiEL=0"),
                 vcov. = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> size:HiEL = 0
#> I(size^2):HiEL = 0
#> I(size^3):HiEL = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ size + I(size^2) + I(size^3) + HiEL + HiEL:size + HiEL:I(size^2) + 
#>     HiEL:I(size^3) + lunch + log(income)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    413                    
#> 2    410  3 2.1885 0.08882 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# scatterplot
# plot(CASchools$size, 
#      CASchools$score, 
#      xlim = c(12, 28),
#      ylim = c(600, 740),
#      pch = 20, 
#      col = "gray", 
#      xlab = "Student-Teacher Ratio", 
#      ylab = "Test Score")

# add a legend
# legend("top", 
#        legend = c("Linear Regression (2)", 
#                   "Cubic Regression (5)", 
#                   "Cubic Regression (7)"),
#        cex = 0.6,
#        ncol = 3,
#        lty = c(1, 1, 2),
#        col = c("blue", "red", "black"))

# data for use with predict()
# new_data <- data.frame("size" = seq(16, 24, 0.05), 
#                        "english" = mean(CASchools$english),
#                        "lunch" = mean(CASchools$lunch),
#                        "income" = mean(CASchools$income),
#                        "HiEL" = mean(CASchools$HiEL))

# add estimated regression function for model (2)
# fitted <- predict(TS_mod2, newdata = new_data)
# 
# lines(new_data$size, 
#       fitted,
#       lwd = 1.5,
#       col = "blue")

# add estimated regression function for model (5)
# fitted <- predict(TS_mod5, newdata = new_data)

# lines(new_data$size, 
#       fitted, 
#       lwd = 1.5,
#       col = "red")

# add estimated regression function for model (7)
# fitted <- predict(TS_mod7, newdata = new_data)
# 
# lines(new_data$size, 
#       fitted, 
#       col = "black",
#       lwd = 1.5,
#       lty = 2)
new_data <- tibble(size = seq(16, 24, 0.05),
                   english = mean(CASchools$english),
                   lunch = mean(CASchools$lunch),
                   income = mean(CASchools$income),
                   HiEL = mean(CASchools$HiEL))

new_data <- new_data %>%
    mutate(fitted2 = predict(TS_mod2, newdata = new_data),
           fitted5 = predict(TS_mod5, newdata = new_data),
           fitted7 = predict(TS_mod7, newdata = new_data))

ggplot(CASchools, aes(x = size, y = score)) +
  geom_point(col = "gray") +
  geom_line(aes(y = fitted2, color = "Linear Regression (2)"), data = new_data) +
  geom_line(aes(y = fitted5, color = "Cubic Regression (5)"), data = new_data) +
  geom_line(aes(y = fitted7, color = "Cubic Regression (7)"), data = new_data) +
  scale_color_manual(name = NULL, 
                     limits = c("Linear Regression (2)", 
                                "Cubic Regression (5)", 
                                "Cubic Regression (7)"),
                     values = c("Linear Regression (2)" = "blue",
                                "Cubic Regression (5)" = "red",
                                "Cubic Regression (7)" = "black")) +
  labs(x = "Student-Teacher Ratio", y = "Test Score",
       title = "Estimated Regression Functions") +
  xlim(12, 28) +
  ylim(600, 740) +
  theme(legend.position = c(0.5, 0.95), legend.direction = "horizontal")


# draw scatterplot

# observations with HiEL = 0
# plot(CASchools$size[CASchools$HiEL == 0], 
#      CASchools$score[CASchools$HiEL == 0], 
#      xlim = c(12, 28),
#      ylim = c(600, 730),
#      pch = 20, 
#      col = "gray", 
#      xlab = "Student-Teacher Ratio", 
#      ylab = "Test Score")

# observations with HiEL = 1
# points(CASchools$size[CASchools$HiEL == 1], 
#        CASchools$score[CASchools$HiEL == 1],
#        col = "steelblue",
#        pch = 20)

# add a legend
# legend("top", 
#        legend = c("Regression (6) with HiEL=0", "Regression (6) with HiEL=1"),
#        cex = 0.7,
#        ncol = 2,
#        lty = c(1, 1),
#        col = c("green", "red"))

# data for use with 'predict()'
# new_data <- data.frame("size" = seq(12, 28, 0.05), 
#                        "english" = mean(CASchools$english),
#                        "lunch" = mean(CASchools$lunch),
#                        "income" = mean(CASchools$income),
#                        "HiEL" = 0)

# add estimated regression function for model (6) with HiEL=0
# fitted <- predict(TS_mod6, newdata = new_data)

# lines(new_data$size, 
#       fitted, 
#       lwd = 1.5,
#       col = "green")

# add estimated regression function for model (6) with HiEL=1
# new_data$HiEL <- 1
 
# fitted <- predict(TS_mod6, newdata = new_data)
 
# lines(new_data$size, 
#       fitted, 
#       lwd = 1.5,
#       col = "red")


new_data <- tibble(size = seq(12, 28, 0.05),
                   english = mean(CASchools$english),
                   lunch = mean(CASchools$lunch),
                   income = mean(CASchools$income),
                   HiEL = 0)

new_data <- new_data %>%
    mutate(fitted0 = predict(TS_mod6, newdata = new_data))    

new_data <- new_data %>%
    mutate(HiEL = 1)

new_data <- new_data %>%
    mutate(fitted1 = predict(TS_mod6, newdata = new_data))


ggplot() +
  geom_point(aes(x = size, y = score), color = "gray",
             data = filter(CASchools, HiEL == 0)) +
  geom_point(aes(x = size, y = score), color = "steelblue",
             data = filter(CASchools, HiEL == 1)) +
  geom_line(aes(x = size, y = fitted0, color = "Regression (6) with HiEL=0"), 
            data = new_data) +
  geom_line(aes(x = size, y = fitted1, color = "Regression (6) with HiEL=1"),
            data = new_data) +
  scale_color_manual(name = NULL,
                     values = c("Regression (6) with HiEL=0" = "green",
                                "Regression (6) with HiEL=1" = "red")) +
  labs(x = "Student-Teacher Ratio", y = "Test Score",
       title = "Estimated Regression Functions") +
  xlim(12, 28) +
  ylim(600, 730) +
  theme(legend.position = c(0.5, 0.95), legend.direction = "horizontal")    


# 8.5 Exercises ----------------------------------------------------------------

# 1. Correlation and (Non)linearity I
# compute the correlation between medv and lstat
cor(Boston$medv, Boston$lstat)

# plot medv against lstat and add the regression line
# plot(Boston$lstat,
#      Boston$medv,
#      pch = 20,
#      col = "steelblue",
#      xlab = "lstat",
#      ylab = "medv")

# abline(lm(medv ~ lstat, data = Boston),
#        col = "red",
#        lwd = 1.5)
ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(col = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "lstat", y = "medv")


# 2. Correlation and (Non)linearity II
# conduct the regression and assign it to mod_log
mod_log <- lm(medv ~ log(lstat), data = Boston)

# draw a scatterplot and add the regression line
# plot(log(Boston$lstat),
#      Boston$medv,
#      pch = 20,
#      col = "steelblue",
#      xlab = "log(lstat)",
#      ylab = "medv")

# abline(mod_log,
#        col = "red",
#        lwd = 1.5)
ggplot(Boston, aes(x = log(lstat), y = medv)) +
  geom_point(col = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "log(lstat)", y = "medv")


# 3. The Optimal Polynomial Order — Sequential Testing 
# find the optimal polynomial order of the polylog model
for(i in 4:1){
    mod  <- lm(medv ~ poly(log(lstat), i, raw = T), data = Boston)
    pval <- coeftest(mod, vcov = vcovHC)[(i+1), 4]
    if(pval < 0.05){
        print(i)
        break
    }
}

# extract the R^2 from the selected model and assign it to R2
R2 <- summary(mod)$r.squared
R2
#> [1] 0.6760386


# 4. The Estimated Effect of a Unit Change
mod_pl <- lm(medv ~ poly(log(lstat), 2, raw = T), data = Boston)
# set up the relevant data points
# new_data <- data.frame(lstat = c(10, 11))
new_data <- tibble(lstat = c(10, 11))

# predict the corresponding values of medv
Y_hat <- predict(mod_pl, newdata = new_data)

# compute the expected effect of the change
diff(Y_hat)
#>         2 
#> -1.157735


# 5. Interactions between Independent Variables I
# generate the binary variable `old` and append it to the dataset
# Boston$old <- ifelse(Boston$age >= 95, 1, 0)
Boston <- Boston %>%
  mutate(old = ifelse(age >= 95, 1, 0))

# conduct the regression and assign it to `mod_bb`
mod_bb <- lm(medv ~ chas + old + chas:old, data = Boston)

# print a robust summary to the console
coeftest(mod_bb, vcov. = vcovHC)
#> t test of coefficients:
#>
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 23.69890    0.43456 54.5349 < 2.2e-16 ***
#> chas         4.05824    2.00689  2.0222   0.04369 *  
#> old         -7.13192    0.93018 -7.6673 9.171e-14 ***
#> chas:old    10.54621    7.55193  1.3965   0.16318  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 6. Interactions between Independent Variables II
# conduct the regression and assign it to mod_bc.
mod_bc <- lm(medv ~ indus*old, data = Boston)

# extract the estimated model coefficients and assign them to params.
params <- coef(mod_bc)

# plot medv against indus and add the regression lines.
# plot(Boston$indus,
#      Boston$medv,
#      pch = 20,
#      col = "steelblue",
#      xlab = "indus",
#      ylab = "medv")

# abline(a = params[1], 
#        b = params[2], 
#        col = "red")
# abline(a = params[1] + params[3], 
#        b = params[2] + params[4], 
#        col = "darkblue")

ggplot(Boston, aes(x = indus, y = medv)) +
  geom_point(col = "steelblue") +
  geom_abline(aes(intercept = params[1], 
                  slope = params[2]), 
              color = "red") +
  geom_abline(aes(intercept = params[1] + params[3], 
                  slope = params[2] + params[4]),
              color = "darkblue") +
  labs(x = "indus", y = "medv")
