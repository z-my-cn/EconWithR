# Econometrics with R
# Chapter 05 Hypothesis Tests and Confidence Intervals in SLR Model ------------
library(AER)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(latex2exp)

# 5.1 Testing Two-Sided Hypotheses Concerning the Slope Coefficient ------------

# load the `CASchools` dataset
data(CASchools)

# add student-teacher ratio
# CASchools$STR <- CASchools$students/CASchools$teachers

# add average test-score
# CASchools$score <- (CASchools$read + CASchools$math)/2

CASchools <- as_tibble(CASchools)
CASchools <- CASchools %>%
  mutate(STR = students/teachers,
         score = (read + math)/2)

# estimate the model
linear_model <- lm(score ~ STR, data = CASchools)          


# print the summary of the coefficients to the console
summary(linear_model)$coefficients
#>               Estimate Std. Error   t value      Pr(>|t|)
#> (Intercept) 698.932949  9.4674911 73.824516 6.569846e-242
#> STR          -2.279808  0.4798255 -4.751327  2.783308e-06


# determine residual degrees of freedom
linear_model$df.residual
#> [1] 418


2 * pt(-4.751327, df = 418)
#> [1] 2.78331e-06


2 * pnorm(-4.751327)
#> [1] 2.02086e-06


# Plot the standard normal on the support [-6,6]
# t <- seq(-6, 6, 0.01)

# plot(x = t, 
#      y = dnorm(t, 0, 1), 
#      type = "l", 
#      col = "steelblue", 
#      lwd = 2, 
#      yaxs = "i", 
#      axes = F, 
#      ylab = "", 
#      main = expression("Calculating the p-value of a Two-sided Test when"
#                        ~ t^act ~ "=-4.75"), 
#      cex.lab = 0.7,
#      cex.main = 1)

# tact <- -4.75

# axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)

# Shade the critical regions using polygon():

# critical region in left tail
# polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
#         y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
#         col = 'orange')

# critical region in right tail
# polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
#         y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
#         col = 'orange')

# Add arrows and texts indicating critical regions and the p-value
# arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
# arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
# 
# arrows(-5, 0.16, -4.75, 0, length = 0.1)
# arrows(5, 0.16, 4.75, 0, length = 0.1)
# 
# text(-3.5, 0.22, 
#      labels = expression("0.025"~"="~over(alpha, 2)),
#      cex = 0.7)
# text(3.5, 0.22, 
#      labels = expression("0.025"~"="~over(alpha, 2)),
#      cex = 0.7)
# 
# text(-5, 0.18, 
#      labels = expression(paste("-|",t[act],"|")), 
#      cex = 0.7)
# text(5, 0.18, 
#      labels = expression(paste("|",t[act],"|")), 
#      cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^act and -t^act 
# rug(c(-1.96, 1.96), ticksize  = 0.145, lwd = 2, col = "darkred")
# rug(c(-tact, tact), ticksize  = -0.0451, lwd = 2, col = "darkgreen")
tact <- -4.75
ggplot() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-6, 6), 
                geom = "line", size = 1.2, color = "steelblue") +
  geom_polygon(aes(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
                   y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0)), 
               fill = "orange", alpha = 0.5) +
  geom_polygon(aes(x = c(1.96, seq(1.96, 6, 0.01), 6),
                   y = c(0, dnorm(seq(1.96, 6, 0.01)), 0)), 
               fill = "orange", alpha = 0.5) +
  geom_segment(aes(x = -3.5, y = 0.2, xend = -2.5, yend = 0.02), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = 3.5, y = 0.2, xend = 2.5, yend = 0.02), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = -5, y = 0.16, xend = -4.75, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = 5, y = 0.16, xend = 4.75, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_text(aes(x = -3.5, y = 0.22), parse = TRUE,
            label = "0.025 == frac(alpha, 2)") +
  geom_text(aes(x = 3.5, y = 0.22), parse = TRUE, 
            label = "0.025 == frac(alpha, 2)") +
  geom_text(aes(x = -5, y = 0.18), parse = TRUE, label = "-abs(t[act])") +
  geom_text(aes(x = 5, y = 0.18), parse = TRUE, label = "abs(t[act])") +
  geom_rug(aes(x = c(-1.96, 1.96)), 
           sides = "b", length = unit(0.145, "cm"), color = "darkred") +
  geom_rug(aes(x = c(-tact, tact)),
           sides = "b", length = unit(-0.0451, "cm"), color = "darkgreen") +
  
  scale_x_continuous(breaks = c(0, -1.96, 1.96, -tact, tact),
                     limits = c(-6, 6)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(t[act]), y = "",
       title = expression("Calculating the p-value of a Two-sided Test when"
                           ~ t^act ~ "=-4.75")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")


# 5.2 Confidence Intervals for Regression Coefficients -------------------------

# set seed for reproducibility
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)

# plot(Y, 
#      pch = 19, 
#      col = "steelblue")
ggplot() +
  geom_point(aes(x = 1:100, y = Y), 
             color = "steelblue", size = 2) +
  labs(x = "Observation", y = "Y", title = "Sample Data")


cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10)
#>       CIlower  CIupper
#> [1,] 4.502625 6.462625


# set seed
set.seed(1)

# initialize vectors of lower and upper interval boundaries
lower <- numeric(10000)
upper <- numeric(10000)

# loop sampling / estimation / CI
for(i in 1:10000) {
    
    Y <- rnorm(100, mean = 5, sd = 5)
    lower[i] <- mean(Y) - 1.96 * 5 / 10
    upper[i] <- mean(Y) + 1.96 * 5 / 10
    
}

# join vectors of interval bounds in a matrix
CIs <- cbind(lower, upper)


mean(CIs[, 1] <= 5 & 5 <= CIs[, 2])
#> [1] 0.9487


# identify intervals not covering mu
# (4 intervals out of 100)
# ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

# initialize the plot
# plot(0, 
#      xlim = c(3, 7), 
#      ylim = c(1, 100), 
#      ylab = "Sample", 
#      xlab = expression(mu), 
#      main = "Confidence Intervals")

# set up color vector
# colors <- rep(gray(0.6), 100)
# colors[ID] <- "red"

# draw reference line at mu=5
# abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
# for(j in 1:100) {
#     
#     lines(c(CIs[j, 1], CIs[j, 2]), 
#           c(j, j), 
#           col = colors[j], 
#           lwd = 2)
#     
# }
df <- tibble(ID = 1:100,
             lower = CIs[1:100, 1],
             upper = CIs[1:100, 2]) %>%
  mutate(color = ifelse(lower <= 5 & 5 <= upper, "gray60", "red"))

ggplot(df) +
  geom_segment(aes(x = lower, xend = upper, y = ID, yend = ID, color = color), 
               size = 1) +
  geom_vline(aes(xintercept = 5), linetype = "dashed") +
  scale_color_identity() +
  xlim(2.8, 7.2) +
  labs(x = expression(mu), y = "Sample", title = "Confidence Intervals") +
  theme(legend.position = "none")


# compute 95% confidence interval for coefficients in 'linear_model'
confint(linear_model)
#>                 2.5 %     97.5 %
#> (Intercept) 680.32312 717.542775
#> STR          -3.22298  -1.336636


# compute 95% confidence interval for coefficients in 'linear_model' by hand
lm_sum <- summary(linear_model)

c("lower" = lm_sum$coef[2,1] - qt(0.975, df = lm_sum$df[2]) * lm_sum$coef[2, 2],
  "upper" = lm_sum$coef[2,1] + qt(0.975, df = lm_sum$df[2]) * lm_sum$coef[2, 2])
#>     lower     upper 
#> -3.222980 -1.336636


# 5.3 Regression when X is a Binary Variable -----------------------------------

# Create the dummy variable as defined above
# CASchools$D <- CASchools$STR < 20
CASchools <- CASchools %>%
  mutate(D = STR < 20)

# Compute the average score when D=1 (low  STR)
# mean.score.for.D.1 <- mean(CASchools$score[CASchools$D == TRUE])
mean.score.for.D.1 <- CASchools %>%
  filter(D == TRUE) %>%
  summarize(mean.score = mean(score)) %>%
  pull(mean.score)

# Compute the average score when D=0 (high STR)
# mean.score.for.D.0 <- mean(CASchools$score[CASchools$D == FALSE])
mean.score.for.D.0 <- CASchools %>%
  filter(D == FALSE) %>%
  summarize(mean.score = mean(score)) %>%
  pull(mean.score)


# plot( CASchools$score ~ CASchools$D,        # provide the data to be plotted
#       pch = 19,                             # use filled circles as plot symbols
#       cex = 0.5,                            # set size of plot symbols to 0.5
#       col = "Steelblue",                    # set the symbols' color to "Steelblue"
#       xlab = expression(D[i]),              # Set title and axis names
#       ylab = "Test Score",
#       main = "Dummy Regression")
# 
# # Add the average for each group
# points( y = mean.score.for.D.0, x = 0,   col="red", pch = 19)
# points( y = mean.score.for.D.1, x = 1,   col="red", pch = 19)
ggplot(CASchools) +
  geom_point(aes(x = D, y = score), 
             color = "steelblue", size = 2) +
  geom_point(aes(x = FALSE, y = mean.score.for.D.0), 
             color = "red", size = 2) +
  geom_point(aes(x = TRUE, y = mean.score.for.D.1), 
             color = "red", size = 2) +
  labs(x = expression(D[i]), y = "Test Score", title = "Dummy Regression")


# estimate the dummy regression model
dummy_model <- lm(score ~ D, data = CASchools)
summary(dummy_model)
#> 
#> Call:
#> lm(formula = score ~ D, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -50.496 -14.029  -0.346  12.884  49.504 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  650.077      1.393 466.666  < 2e-16 ***
#> DTRUE          7.169      1.847   3.882  0.00012 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 18.74 on 418 degrees of freedom
#> Multiple R-squared:  0.0348, Adjusted R-squared:  0.0325 
#> F-statistic: 15.07 on 1 and 418 DF,  p-value: 0.0001202


# add group specific predictions to the plot
# points(x = CASchools$D, 
#        y = predict(dummy_model), 
#        col = "red", 
#        pch = 20)
ggplot(CASchools) +
  geom_point(aes(x = D, y = score),
             color = "steelblue", size = 2) +
  geom_point(aes(x = FALSE, y = mean.score.for.D.0),
             color = "red", size = 2) +
  geom_point(aes(x = TRUE, y = mean.score.for.D.1),
             color = "red", size = 2) +
  geom_point(aes(x = D, y = predict(dummy_model)), 
             color = "red", size = 2) +
  labs(x = expression(D[i]), y = "Test Score", title = "Dummy Regression")


# confidence intervals for coefficients in the dummy regression model
confint(dummy_model)
#>                  2.5 %    97.5 %
#> (Intercept) 647.338594 652.81500
#> DTRUE         3.539562  10.79931


# 5.4 Heteroskedasticity and Homoskedasticity ----------------------------------

# load scales package for adjusting color opacities
# library(scales)

# generate some heteroskedastic data:

# set seed for reproducibility
set.seed(123) 

# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25)

# initialize vector of errors
e <- c()

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10)
e[26:50] <- rnorm(25, sd = 15)
e[51:75] <- rnorm(25, sd = 20)
e[76:100] <- rnorm(25, sd = 25)

# set up y
y <- 720 - 3.3 * x + e

# Estimate the model 
mod <- lm(y ~ x)

# Plot the data
# plot(x = x, 
#      y = y, 
#      main = "An Example of Heteroskedasticity",
#      xlab = "Student-Teacher Ratio",
#      ylab = "Test Score",
#      cex = 0.5, 
#      pch = 19, 
#      xlim = c(8, 27), 
#      ylim = c(600, 710))

# Add the regression line to the plot
# abline(mod, col = "darkred")

# Add boxplots to the plot
# boxplot(formula = y ~ x, 
#         add = TRUE, 
#         at = c(10, 15, 20, 25), 
#         col = alpha("gray", 0.4), 
#         border = "black")
ggplot() +
  geom_point(aes(x = x, y = y), 
             color = "steelblue", size = 2) +
  geom_smooth(aes(x = x, y = y), formula = y ~ x,
              method = "lm", se = FALSE, color = "darkred") +
  geom_boxplot(aes(x = x, y = y, group = x),
               color = "black", fill = alpha("gray", 0.4)) +
  labs(x = "Student-Teacher Ratio", y = "Test Score", 
       title = "An Example of Heteroskedasticity") +
  xlim(8, 27) +
  ylim(600, 710)


# load package and attach data
# library(AER)
data("CPSSWEducation")
# attach(CPSSWEducation)
CPSSWEducation <- as_tibble(CPSSWEducation)

# get an overview
summary(CPSSWEducation)
#>       age          gender        earnings        education    
#>  Min.   :29.0   female:1202   Min.   : 2.137   Min.   : 6.00  
#>  1st Qu.:29.0   male  :1748   1st Qu.:10.577   1st Qu.:12.00  
#>  Median :29.0                 Median :14.615   Median :13.00  
#>  Mean   :29.5                 Mean   :16.743   Mean   :13.55  
#>  3rd Qu.:30.0                 3rd Qu.:20.192   3rd Qu.:16.00  
#>  Max.   :30.0                 Max.   :97.500   Max.   :18.00

# estimate a simple regression model
# labor_model <- lm(earnings ~ education)
labor_model <- lm(earnings ~ education, data = CPSSWEducation)

# plot observations and add the regression line
# plot(education, 
#      earnings,
#      xlab="Earnings",
#      ylab="Education",
#      ylim = c(0, 150))
# 
# abline(labor_model, 
#        col = "steelblue", 
#        lwd = 2)
ggplot(CPSSWEducation) +
  geom_point(aes(x = education, y = earnings), 
             color = "steelblue", size = 2) +
  geom_smooth(aes(x = education, y = earnings), formula = y ~ x,
              method = "lm", se = FALSE, color = "darkred") +
  labs(x = "Education", y = "Earnings") +
  ylim(0, 150)


# print the contents of labor_model to the console
labor_model
#> 
#> Call:
#> lm(formula = earnings ~ education, data = CPSSWEducation)
#> 
#> Coefficients:
#> (Intercept)    education  
#>      -3.134        1.467


# compute a 95% confidence interval for the coefficients in the model
confint(labor_model)
#>                 2.5 %    97.5 %
#> (Intercept) -5.015248 -1.253495
#> education    1.330098  1.603753


# Store model summary in 'model'
model <- summary(labor_model)

# Extract the standard error of the regression from model summary
SER <- model$sigma

# Compute the variation in 'education'
# V <- (nrow(CPSSWEducation)-1) * var(education)
V <- CPSSWEducation %>%
  summarize(V = (n() - 1) * var(education)) %>%
  pull(V)

# Compute the standard error of the slope parameter's estimator and print it
SE.beta_1.hat <- sqrt(SER^2/V)
SE.beta_1.hat
#> [1] 0.06978281

# Use logical operators to see if the value computed by hand matches the one provided 
# in mod$coefficients. Round estimates to four decimal places
round(model$coefficients[2, 2], 4) == round(SE.beta_1.hat, 4)
#> [1] TRUE


# compute heteroskedasticity-robust standard errors
vcov <- vcovHC(linear_model, type = "HC1")
vcov
#>             (Intercept)        STR
#> (Intercept)  107.419993 -5.3639114
#> STR           -5.363911  0.2698692


# compute the square root of the diagonal elements in vcov
robust_se <- sqrt(diag(vcov))
robust_se
#> (Intercept)         STR 
#>  10.3643617   0.5194893


# we invoke the function `coeftest()` on our model
coeftest(linear_model, vcov. = vcov)
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 698.93295   10.36436 67.4362 < 2.2e-16 ***
#> STR          -2.27981    0.51949 -4.3886 1.447e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


set.seed(905)

# generate heteroskedastic data 
X <- 1:500
Y <- rnorm(n = 500, mean = X, sd = 0.6 * X)

# estimate a simple regression model
reg <- lm(Y ~ X)


# plot the data
# plot(x = X, y = Y, 
#      pch = 19, 
#      col = "steelblue", 
#      cex = 0.8)

# add the regression line to the plot
# abline(reg, 
#        col = "red", 
#        lwd = 1.5)
# legend("topleft","Regression line",col="red",lwd=1.5)
ggplot() +
  geom_point(aes(x = X, y = Y), 
             color = "steelblue", size = 2) +
  geom_smooth(aes(x = X, y = Y, color = "Regression line"),
              formula = y ~ x, method = "lm", se = FALSE) +
  labs(x = "X", y = "Y", color = NULL) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# test hypthesis using the default standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1")$'Pr(>F)'[2] < 0.05
#> [1] TRUE

# test hypothesis using the robust standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1",
                 white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
#> [1] FALSE



# initialize vectors t and t.rob
t <- c()
t.rob <- c()

# loop sampling and estimation
for (i in 1:10000) {
    
    # sample data
    X <- 1:1000
    Y <- rnorm(n = 1000, mean = X, sd = 0.6 * X)
    
    # estimate regression model
    reg <- lm(Y ~ X)
    
    # homoskedasdicity-only significance test
    t[i] <- linearHypothesis(reg, "X = 1")$'Pr(>F)'[2] < 0.05
    
    # robust significance test
    t.rob[i] <- linearHypothesis(reg, "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
    
}

# compute the fraction of false rejections
round(cbind(t = mean(t), t.rob = mean(t.rob)), 3)
#>          t t.rob
#> [1,] 0.073  0.05


# 5.5 The Gauss-Markov Theorem -------------------------------------------------

# set sample size and number of repetitions
n <- 100      
reps <- 1e5

# choose epsilon and create a vector of weights as defined above
epsilon <- 0.8
w <- c(rep((1 + epsilon) / n, n / 2), 
       rep((1 - epsilon) / n, n / 2) )

# draw a random sample y_1,...,y_n from the standard normal distribution, 
# use both estimators 1e5 times and store the result in the vectors 'ols' and 
# 'weightedestimator'

ols <- rep(NA, reps)
weightedestimator <- rep(NA, reps)

for (i in 1:reps) {
    
    y <- rnorm(n)
    ols[i] <- mean(y)
    weightedestimator[i] <- crossprod(w, y)
    
}

# plot kernel density estimates of the estimators' distributions: 

# OLS
# plot(density(ols), 
#      col = "red", 
#      lwd = 2, 
#      main = "Density of OLS and Weighted Estimator",
#      xlab = "Estimates")

# weighted
# lines(density(weightedestimator), 
#       col = "steelblue", 
#       lwd = 2) 

# add a dashed line at 0 and add a legend to the plot
# abline(v = 0, lty = 2)
# 
# legend('topright', 
#        c("OLS", "Weighted"), 
#        col = c("red", "steelblue"), 
#        lwd = 2)
ggplot() +
  geom_density(aes(x = ols, color = "OLS"), linewidth = 2) +
  geom_density(aes(x = weightedestimator, color = "Weighted"), linewidth = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("OLS" = "red", "Weighted" = "steelblue")) +
  labs(x = "Estimates", title = "Density of OLS and Weighted Estimator",
       color = NULL) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))


# 5.6 Using the t-Statistic in Regression When the Sample Size Is Small --------

# initialize two vectors
beta_0 <- c()
beta_1 <- c()

# loop sampling / estimation / t statistics
for (i in 1:10000) {
    
    X <- runif(20, 0, 20)
    Y <- rnorm(n = 20, mean = X)
    reg <- summary(lm(Y ~ X))
    beta_0[i] <- (reg$coefficients[1, 1] - 0)/(reg$coefficients[1, 2])
    beta_1[i] <- (reg$coefficients[2, 1] - 1)/(reg$coefficients[2, 2])
    
}

# plot the distributions and compare with t_18 density:

# divide plotting area
# par(mfrow = c(1, 2))

# plot the simulated density of beta_0
# plot(density(beta_0), 
#      lwd = 2 , 
#      main = expression(widehat(beta)[0]), 
#      xlim = c(-4, 4),
#      ylim=c(0,0.5))

# add the t_18 density to the plot
# curve(dt(x, df = 18), 
#       add = T, 
#       col = "red", 
#       lwd = 2, 
#       lty = 2)

# plot the simulated density of beta_1
# plot(density(beta_1), 
#      lwd = 2, 
#      main = expression(widehat(beta)[1]),
#      xlim = c(-4, 4),
#      ylim=c(0,0.5))

# add the t_18 density to the plot
# curve(dt(x, df = 18), 
#       add = T, 
#       col = "red", 
#       lwd = 2, 
#       lty = 2) 
p1 <- ggplot() +
  geom_density(aes(x = beta_0), linewidth = 2) +
  stat_function(fun = dt, args = list(df = 18), 
                geom = "line", linewidth = 2, 
                color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Density", title = expression(widehat(beta)[0])) +
  ylim(0, 0.5)

p2 <- ggplot() +
  geom_density(aes(x = beta_1), linewidth = 2) +
  stat_function(fun = dt, args = list(df = 18), 
                geom = "line", linewidth = 2, 
                color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Density", title = expression(widehat(beta)[1])) +
  ylim(0, 0.5)

p1 + p2


# 5.7 Exercises ----------------------------------------------------------------

# 1. Testing Two Null Hypotheses Separately 
TS <- c(430, 430, 333, 410, 390, 377, 325, 310, 328, 375)
STR <- c(23, 19, 30, 22, 23, 29, 35, 36, 33, 25)
mod <- lm(TS ~ STR)
summary(mod)
# Call:
# lm(formula = TS ~ STR)

# Residuals:
#     Min       1Q   Median       3Q      Max 
# -19.9248 -10.6002  -0.8506   5.8631  27.0246 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 567.4272    23.9606  23.682 1.08e-08 ***
# STR          -7.1501     0.8536  -8.376 3.13e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 15.19 on 8 degrees of freedom
# Multiple R-squared:  0.8976,	Adjusted R-squared:  0.8849 
# F-statistic: 70.16 on 1 and 8 DF,  p-value: 3.132e-05

# compute the p-value for the first significance test and save it to p_int
t_int <- 567.43/23.9606
p_int <- 2*(1-pnorm(abs(t_int)))
p_int
#> [1] 0

# compute the p-value for the second significance test and save it to p_STR
t_STR <- 7.15/0.8536
p_STR <- 2*(1-pnorm(abs(t_STR)))
p_STR
#> [1] 0


# 2. Two Null Hypotheses You Cannot Reject, Can You?
test <- c(t_int, t_STR)
# entry is `TRUE` if a test rejects
abs(test) >= qnorm(0.975)
#> [1]  TRUE  TRUE


# 3. Confidence Intervals
# compute 90% confidence intervals for the model coefficients
confint(mod, level = 0.9)
#>                   5 %       95 %
#> (Intercept) 522.87120 611.983142
#> STR          -8.73742  -5.562738


# 4. A Confidence Interval for the Mean I
# set seed for reproducibility
set.seed(1)


# sample the observations
sample <- rnorm(100, mean = 10, sd = 10)

# estimate the mean and assign the estimate to `mu_hat`
mu_hat <- mean(sample)

# compute the 95% confidence interval using the formula above
CI <- c(
    "lower" = mu_hat - qnorm(0.975) * 10 / sqrt(100),
    "upper" = mu_hat + qnorm(0.975) * 10 / sqrt(100)
)
CI
#>   lower    upper 
#> 9.12891 13.04884 


# 5. A Confidence Interval for the Mean II
# use a regression to obtain an estimate of the mean
linear_model <- lm(sample ~ 1)

# compute the 95% CI for the mean
confint(linear_model)
#                2.5 %  97.5 %
# (Intercept) 9.306651 12.8711


# 6. Regression on a Dummy Variable I
DF <- read_csv("data/Exercises_CH05/DF.csv")

# generate the dummy vector `dummy` using `ifelse()`
# dummy <- ifelse(DF$X > 0, 1, 0)

# append `dummy` to `DF`
# DF <- cbind(DF, dummy)
DF <- DF %>%
  mutate(dummy = ifelse(X > 0, 1, 0))

head(DF)
# # A tibble: 6 × 3
#        Y      X dummy
#    <dbl>  <dbl> <dbl>
# 1 -0.626 -0.626     0
# 2  0.184  0.184     1
# 3 -0.836 -0.836     0
# 4  1.60   1.60      1
# 5  0.330  0.330     1
# 6 -0.820 -0.820     0


# 7. Regression on a Dummy Variable II
# mu_Y_D1 <- mean(DF$Y[DF$D ==1])
# mu_Y_D0 <- mean(DF$Y[DF$D ==0])
# lm(Y ~ D, data = DF)
mu_Y_D1 <- DF %>%
  filter(dummy == 1) %>%
  summarize(mu_Y_D1 = mean(Y)) %>%
  pull(mu_Y_D1)
mu_Y_D0 <- DF %>%
  filter(dummy == 0) %>%
  summarize(mu_Y_D0 = mean(Y)) %>%
  pull(mu_Y_D0)

dummy_mod <- lm(Y ~ dummy, data = DF)
dummy_mod
#> Call:
#> lm(formula = Y ~ dummy, data = DF)
#> 
#> Coefficients:
#> (Intercept)        dummy  
#>     -0.6607       1.4251  

# 8. Regression on a Dummy Variable III
# Replace the `???` by the correct values
# plot(x = ??? , y = ???, 
#      pch = 20, 
#      cex = 1,
#      col = "Steelblue",
#      xlab = expression(D[i]), ylab = "Test Score",
#      main = "Dummy Regression"
# )
# plot(x = DF$dummy, y = DF$Y, 
#      pch = 20, 
#      cex = 1,
#      col = "Steelblue",
#      xlab = expression(D[i]), ylab = "Test Score",
#      main = "Dummy Regression"
# )

# add the regression line
# abline(dummy_mod,
#        col = "red",
#        lwd = 2)
ggplot(DF) +
  geom_point(aes(x = dummy, y = Y), 
             color = "steelblue", size = 2) +
  geom_smooth(aes(x = dummy, y = Y), formula = y ~ x,
              method = "lm", se = FALSE, color = "darkred") +
  labs(x = expression(D[i]), y = "Test Score", title = "Dummy Regression")


# 9. Gender Wage Gap I
# load the package and attach the data set
data("CPS1985")
# attach(CPS1985)
CPS1985 <- as_tibble(CPS1985)

# perform the regression and assign the result to `dummy_mod`
dummy_mod <- lm(wage~gender, data = CPS1985)
dummy_mod
#> Call:
#> lm(formula = wage ~ gender, data = CPS1985)
#> 
#> Coefficients:
#> (Intercept)  genderfemale  
#>       9.995        -2.116


# 10. Gender Wage Gap II
# test whether the gender gap is significantly different from zero 
# use robust standard errors
coeftest(dummy_mod, vcov. = vcovHC(dummy_mod, type = "HC0"))
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)   9.99491    0.31039 32.2007 < 2.2e-16 ***
#> genderfemale -2.11606    0.43233 -4.8945 1.308e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 11. Computation of Heteroskedasticity-Robust Standard Errors
# compute robust standard errors and save them in rob_SEs
rob_SEs <- sqrt(diag(vcovHC(dummy_mod, type = "HC1")))
rob_SEs
#> (Intercept) genderfemale 
#>   0.3109771    0.4331428


# 12. Robust Confidence Intervals
# complete the function below
# Rob_CI <- function(model) {
#     SEs <- ???
#         lower <- model$coef - ???
#             upper <- model$coef + ???
#                 return(
#                     cbind("Lower" = lower, "Upper" = upper)
#                 )
# }
Rob_CI <- function(model) {
    SEs <- sqrt(diag(vcovHC(model, type = "HC1")))
    lower <- model$coef - 1.96 * SEs
    upper <- model$coef + 1.96 * SEs
    return(
        cbind("Lower" = lower, "Upper" = upper)
    )
}
# use Rob_CI() to compute the confidence intervals for both model coefficients
Rob_CI(dummy_mod)


# 13. A Small Simulation Study — I
# write the function `GDP_OLS`
DGP_OLS <- function() {
    X <- runif(100,2,10)
    Y <- 2*X + rnorm(100, sd = sqrt(X))
    return(
        c("beta_1_hat" = sum(X*Y)/sum(X^2))
    )
}


# 14. A Small Simulation Study — II
set.seed(1)
# generate 1000 estimates of beta_1 using `DGP_OLS()` and store them in `estimates`
estimates <- replicate(1000, DGP_OLS())

# estimate the variance of the estimator and assign the result to `est_var_OLS`
est_var_OLS <- var(estimates)
est_var_OLS
#> [1] 0.001805372


# 15. A Small Simulation Study — III
set.seed(1)
# define the function `DGP_WLS()`
DGP_WLS <- function() {
    X <- runif(100,2,10)
    Y <- 2*X + rnorm(100, sd = sqrt(X))
    w <- 1/sqrt(X)
    return(
        c("beta_1_hat" = sum(w^2*X*Y)/sum((w*X)^2))
    )
}

# estimate the variance, assign the value to `var_est_WLS`
est_var_WLS <- var(replicate(1000, DGP_WLS()))

# compare the estimated variances
est_var_WLS < est_var_OLS
#> [1] TRUE