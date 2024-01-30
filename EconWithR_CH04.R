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


