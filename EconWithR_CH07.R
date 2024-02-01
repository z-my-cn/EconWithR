# Econometrics with R
# Chapter 07 Hypothesis Tests and Confidence Intervals in MR Models ------------
library(AER)
library(MASS)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(patchwork)


# 7.1 Hypothesis Tests and Confidence Intervals for a Single Coefficient -------

data("CASchools")
CASchools <- as_tibble(CASchools)
CASchools <- CASchools %>% 
    mutate(STR = students/teachers,
           score = (read + math)/2)


model <- lm(score ~ STR + english, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error  t value Pr(>|t|)    
#> (Intercept) 686.032245   8.728225  78.5993  < 2e-16 ***
#> STR          -1.101296   0.432847  -2.5443  0.01131 *  
#> english      -0.649777   0.031032 -20.9391  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute two-sided p-value
2 * (1 - pt(abs(coeftest(model, vcov. = vcovHC, type = "HC1")[2, 3]),
            df = model$df.residual))
#> [1] 0.01130921


# 7.2 An Application to Test Scores and the Student-Teacher Ratio --------------

model <- lm(score ~ STR + english, data = CASchools)
confint(model)
#>                   2.5 %      97.5 %
#> (Intercept) 671.4640580 700.6004311
#> STR          -1.8487969  -0.3537944
#> english      -0.7271113  -0.5724424


confint(model, level = 0.9)
#>                     5 %        95 %
#> (Intercept) 673.8145793 698.2499098
#> STR          -1.7281904  -0.4744009
#> english      -0.7146336  -0.5849200


# compute robust standard errors
rob_se <- diag(vcovHC(model, type = "HC1"))^0.5

# compute robust 95% confidence intervals
rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)
#>       (Intercept)        STR    english
#> lower    668.9252 -1.9496606 -0.7105980
#> upper    703.1393 -0.2529307 -0.5889557

# compute robust 90% confidence intervals

rbind("lower" = coef(model) - qnorm(0.95) * rob_se,
      "upper" = coef(model) + qnorm(0.95) * rob_se)
#>       (Intercept)        STR    english
#> lower    671.6756 -1.8132659 -0.7008195
#> upper    700.3889 -0.3893254 -0.5987341


# scale expenditure to thousands of dollars
# CASchools$expenditure <- CASchools$expenditure/1000
CASchools <- CASchools %>% 
    mutate(expenditure = expenditure/1000)

# estimate the model
model <- lm(score ~ STR + english + expenditure, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error  t value Pr(>|t|)    
#> (Intercept) 649.577947  15.458344  42.0212  < 2e-16 ***
#> STR          -0.286399   0.482073  -0.5941  0.55277    
#> english      -0.656023   0.031784 -20.6398  < 2e-16 ***
#> expenditure   3.867901   1.580722   2.4469  0.01482 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute the sample correlation between 'STR' and 'expenditure'
cor(CASchools$STR, CASchools$expenditure)
#> [1] -0.6199822


# 7.3 Joint Hypothesis Testing Using the F-Statistic ---------------------------

# estimate the multiple regression model
model <- lm(score ~ STR + english + expenditure, data = CASchools)

# execute the function on the model object and provide both linear restrictions 
# to be tested as strings
linearHypothesis(model, c("STR=0", "expenditure=0"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> STR = 0
#> expenditure = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + english + expenditure
#> 
#>   Res.Df   RSS Df Sum of Sq      F   Pr(>F)    
#> 1    418 89000                                 
#> 2    416 85700  2    3300.3 8.0101 0.000386 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# heteroskedasticity-robust F-test
linearHypothesis(model, c("STR=0", "expenditure=0"), white.adjust = "hc1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> STR = 0
#> expenditure = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + english + expenditure
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1    418                      
#> 2    416  2 5.4337 0.004682 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# execute the function on the model object and provide the restrictions 
# to be tested as a character vector
linearHypothesis(model, c("STR=0", "english=0", "expenditure=0"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> STR = 0
#> english = 0
#> expenditure = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + english + expenditure
#> 
#>   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#> 1    419 152110                                  
#> 2    416  85700  3     66410 107.45 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Access the overall F-statistic from the model's summary
summary(model)$fstatistic
#>    value    numdf    dendf 
#> 107.4547   3.0000 416.0000


# 7.4 Confidence Sets for Multiple Coefficients -------------------------------

# draw the 95% confidence set for coefficients on STR and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"),
                  main = "95% Confidence Set",
                  ylab="Coefficients of Expenditure",
                  xlab="Coefficients of STR")


# draw the robust 95% confidence set for coefficients on STR and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red",
                  ylab="Coefficients of Expenditure",
                  xlab="Coefficients of STR")


# draw the 95% confidence set for coefficients on STR and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"),
                  add = T)


# draw the robust 95% confidence set for coefficients on STR and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red",
                  ylab="Coefficients of Expenditure",
                  xlab="Coefficients of STR")

# draw the 95% confidence set for coefficients on STR and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("STR", "expenditure"),
                  add = T)


# 7.5 Model Specification for Multiple Regression ------------------------------

# estimate the model and print the summary to console
model <- lm(score ~ STR + english + lunch, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 700.149957   5.568453 125.7351 < 2.2e-16 ***
#> STR          -0.998309   0.270080  -3.6963 0.0002480 ***
#> english      -0.121573   0.032832  -3.7029 0.0002418 ***
#> lunch        -0.547345   0.024107 -22.7046 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# set seed for reproducibility
set.seed(1)

# generate observations for parking lot space
# CASchools$PLS <- c(22 * CASchools$income 
#                    - 15 * CASchools$STR 
#                    + 0.2 * CASchools$expenditure
#                    + rnorm(nrow(CASchools), sd = 80) + 3000)
CASchools <- CASchools %>% 
    mutate(PLS = 22 * income - 15 * STR + 0.2 * expenditure + 
               rnorm(nrow(CASchools), sd = 80) + 3000)


# plot parking lot space against test score
# plot(CASchools$PLS, 
#      CASchools$score,
#      xlab = "Parking Lot Space",
#      ylab = "Test Score",
#      pch = 20,
#      col = "steelblue")
ggplot(CASchools, aes(x = PLS, y = score)) +
    geom_point(color = "steelblue") +
    labs(x = "Parking Lot Space",
         y = "Test Score")


# regress test score on PLS
summary(lm(score ~ PLS, data = CASchools))
#> 
#> Call:
#> lm(formula = score ~ PLS, data = CASchools)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -42.372  -9.742   0.592  10.481  36.867  
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 4.575e+02  1.171e+01   39.07   <2e-16 ***
#> PLS         6.453e-02  3.836e-03   16.82   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.73 on 418 degrees of freedom
#> Multiple R-squared:  0.4037,	Adjusted R-squared:  0.4022
#> F-statistic:   283 on 1 and 418 DF,  p-value: < 2.2e-16


# 7.6 Analysis of the Test Score Data Set --------------------------------------

# estimate the correlation between 'calworks' and 'lunch'
cor(CASchools$calworks, CASchools$lunch)
#> [1] 0.7394218


# set up arrangement of plots
# m <- rbind(c(1, 2), c(3, 0))
# graphics::layout(mat = m)

# scatterplots
# plot(score ~ english, 
#      data = CASchools, 
#      col = "steelblue", 
#      pch = 20, 
#      xlim = c(0, 100),
#      cex.main = 0.7,
#      xlab="English",
#      ylab="Score",
#      main = "Percentage of English language learners")
# 
# plot(score ~ lunch, 
#      data = CASchools, 
#      col = "steelblue", 
#      pch = 20,
#      cex.main = 0.7,
#      xlab="Lunch",
#      ylab="Score",
#      main = "Percentage qualifying for reduced price lunch")
# 
# plot(score ~ calworks, 
#      data = CASchools, 
#      col = "steelblue", 
#      pch = 20, 
#      xlim = c(0, 100),
#      cex.main = 0.7,
#      xlab="CalWorks",
#      ylab="Score",
#      main = "Percentage qualifying for income assistance")
p1 <- ggplot(CASchools, aes(x = english, y = score)) +
    geom_point(color = "steelblue") +
    labs(x = "English", y = "Score", 
         title = "Percentage of English language learners") +
    xlim(0, 100)

p2 <- ggplot(CASchools, aes(x = lunch, y = score)) +
    geom_point(color = "steelblue") +
    labs(x = "Lunch", y = "Score", 
         title = "Percentage qualifying for reduced price lunch")

p3 <- ggplot(CASchools, aes(x = calworks, y = score)) +
    geom_point(color = "steelblue") +
    labs(x = "CalWorks", y = "Score", 
         title = "Percentage qualifying for income assistance") +
    xlim(0, 100)
    
p1 + p2 + p3 + plot_layout(ncol = 2)


# estimate correlation between student characteristics and test scores
cor(CASchools$score, CASchools$english)
#> [1] -0.6441238
cor(CASchools$score, CASchools$lunch)
#> [1] -0.868772
cor(CASchools$score, CASchools$calworks)
#> [1] -0.6268533


# load the stargazer library
# library(stargazer)

# estimate different model specifications
spec1 <- lm(score ~ STR, data = CASchools)
spec2 <- lm(score ~ STR + english, data = CASchools)
spec3 <- lm(score ~ STR + english + lunch, data = CASchools)
spec4 <- lm(score ~ STR + english + calworks, data = CASchools)
spec5 <- lm(score ~ STR + english + lunch + calworks, data = CASchools)

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
               sqrt(diag(vcovHC(spec2, type = "HC1"))),
               sqrt(diag(vcovHC(spec3, type = "HC1"))),
               sqrt(diag(vcovHC(spec4, type = "HC1"))),
               sqrt(diag(vcovHC(spec5, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"), 
          type = "html", out = "output/table_CH07_1.html")
# Open the file in your browser to view the table


# 7.7 Exercises ----------------------------------------------------------------

# 1. Hypothesis Testing in a Multiple Regression Model - t-statistics and p-values
data("Boston")
Boston <- as_tibble(Boston)

# compute t-statistics for all coefficients. Assign them to `tstats`
mod <- lm(medv ~ lstat + crim + age, data = Boston)

coefs <- coef(mod)
coefs
#> (Intercept)       lstat        crim         age 
#> 32.82804470 -0.99409094 -0.08262222  0.03764664 

SEs <- sqrt(diag(vcov(mod)))
SEs
#> (Intercept)       lstat        crim         age 
#>  0.74773846  0.05075132  0.03594347  0.01224832 

tstats <- coefs/SEs
tstats
# (Intercept)       lstat        crim         age 
#   43.903111  -19.587488   -2.298671    3.073617 


# compute p-values for all significance tests. Assign them to `pval`
pval <- 2 * (pnorm(-abs(tstats)))
pval
#>  (Intercept)        lstat         crim          age 
#> 0.000000e+00 1.977055e-85 2.152362e-02 2.114809e-03

# check whether the hypotheses are rejected at the 1% significance level
pval < 0.01
#> (Intercept)       lstat        crim         age
#>       TRUE         TRUE       FALSE        TRUE


# 2. Hypothesis Testing in a Multiple Regression Model - Confidence Intervals
# construct a 99% confidence interval for all coefficients
confint(mod, level = 0.99)
#>                    0.5 %      99.5 %
#> (Intercept) 30.894648500 34.76144090
#> lstat       -1.125316516 -0.86286535
#> crim        -0.175559754  0.01031531
#> age          0.005976671  0.06931660


# 3. Robust Hypothesis Testing in Multiple Regression Models
# print a coefficient summary that reports robust standard errors
coeftest(mod, vcov. = vcovHC)
#> t test of coefficients:
# 
#>              Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 32.828045   0.746060  44.0019 < 2.2e-16 ***
#> lstat       -0.994091   0.081360 -12.2185 < 2.2e-16 ***
#> crim        -0.082622   0.028295  -2.9200  0.003658 ** 
#> age          0.037647   0.016633   2.2634  0.024035 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# check whether the hypotheses are rejected at the 1% significance level
coeftest(mod, vcov. = vcovHC)[, 4] < 0.01
#> (Intercept)       lstat        crim         age 
#>        TRUE        TRUE        TRUE       FALSE 


# 4. Joint Hypothesis Testing - F-Test I
# estimate the restricted model and save it in `model_res`
model_res <- lm(medv ~ lstat + I(crim + age), data = Boston)

# compute the SSR of the restricted model and assign it to `RSSR`
RSSR <- sum(residuals(model_res)^2)

# estimate the unrestricted model and save it in `model_unres`
model_unres <- lm(medv ~ lstat + crim + age, data = Boston)

# compute the SSR of the unrestricted model and assign it to `USSR`
USSR <- sum(model_unres$residuals^2)


# 5. Joint Hypothesis Testing - F-Test II
# compute the F-statistic and assign it to `Fstat`
Fstat <- ((RSSR-USSR)/1)/(USSR/(nrow(Boston)-3-1))

# compute the p-value and assign it to `pval`
pval <- 1 - pf(Fstat, df1 = 1, df2 = nrow(Boston)-3-1)

# check whether the null is rejected at the 1% significance level
pval < 0.01

# verify your result with `linearHypothesis()`
linearHypothesis(model_unres, "age = crim")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> - crim  + age = 0
#> 
#> Model 1: restricted model
#> Model 2: medv ~ lstat + crim + age
#> 
#>   Res.Df   RSS Df Sum of Sq      F   Pr(>F)   
#> 1    503 19324                                
#> 2    502 18969  1    355.14 9.3989 0.002288 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 6. Joint Hypothesis Testing - Confidence Set
# construct a 99% confidence set for the coefficients of `crim` and `lstat`
confidenceEllipse(mod, which.coef = c("crim", "lstat"), levels = 0.99)

# conduct the corresponding F-test
linearHypothesis(mod, c("crim = 0", "lstat = 0"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> crim = 0
#> lstat = 0
#> 
#> Model 1: restricted model
#> Model 2: medv ~ lstat + crim + age
#> 
#>   Res.Df   RSS Df Sum of Sq      F    Pr(>F)
#> 1    504 36647
#> 2    502 18968  2     17678 233.92 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1