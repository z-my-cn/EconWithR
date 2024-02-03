# Econometrics with R
# Chapter 11 Regression with a Binary Dependent Variable -----------------------
library(AER)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(corrplot)


# 11.1 Binary Dependent Variables and the Linear Probability Model -------------

# load `AER` package and attach the HMDA data
# library(AER)
data(HMDA)
HMDA <- tibble(HMDA)


# inspect the data
head(HMDA)
#> # A tibble: 6 × 14
#>   deny  pirat hirat lvrat chist mhist phist unemp selfemp insurance condomin
#>   <fct> <dbl> <dbl> <dbl> <fct> <fct> <fct> <dbl> <fct>   <fct>     <fct>   
#> 1 no    0.221 0.221 0.8   5     2     no     3.90 no      no        no      
#> 2 no    0.265 0.265 0.922 2     2     no     3.20 no      no        no      
#> 3 no    0.372 0.248 0.920 1     2     no     3.20 no      no        no      
#> 4 no    0.32  0.25  0.860 1     2     no     4.30 no      no        no      
#> 5 no    0.36  0.35  0.6   1     1     no     3.20 no      no        no      
#> 6 no    0.24  0.17  0.511 1     1     no     3.90 no      no        no      
#> # ℹ 3 more variables: afam <fct>, single <fct>, hschool <fct>
summary(HMDA)
#>   deny          pirat            hirat            lvrat        chist   
#>  no :2095   Min.   :0.0000   Min.   :0.0000   Min.   :0.0200   1:1353  
#>  yes: 285   1st Qu.:0.2800   1st Qu.:0.2140   1st Qu.:0.6527   2: 441  
#>             Median :0.3300   Median :0.2600   Median :0.7795   3: 126  
#>             Mean   :0.3308   Mean   :0.2553   Mean   :0.7378   4:  77  
#>             3rd Qu.:0.3700   3rd Qu.:0.2988   3rd Qu.:0.8685   5: 182  
#>             Max.   :3.0000   Max.   :3.0000   Max.   :1.9500   6: 201  
#>  mhist    phist          unemp        selfemp    insurance  condomin  
#>  1: 747   no :2205   Min.   : 1.800   no :2103   no :2332   no :1694  
#>  2:1571   yes: 175   1st Qu.: 3.100   yes: 277   yes:  48   yes: 686  
#>  3:  41              Median : 3.200                                   
#>  4:  21              Mean   : 3.774                                   
#>                      3rd Qu.: 3.900                                   
#>                      Max.   :10.600                                   
#>   afam      single     hschool   
#>  no :2041   no :1444   no :  39  
#>  yes: 339   yes: 936   yes:2341  
#>                                  
#>                                  
#>                                  
#> 


# convert 'deny' to numeric
# HMDA$deny <- as.numeric(HMDA$deny) - 1
HMDA <- HMDA %>% 
    mutate(deny = as.numeric(deny) - 1)

# estimate a simple linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1
#> 
#> Call:
#> lm(formula = deny ~ pirat, data = HMDA)
#> 
#> Coefficients:
#> (Intercept)        pirat  
#>    -0.07991      0.60353


# plot the data
# plot(x = HMDA$pirat, 
#      y = HMDA$deny,
#      main = "Scatterplot Mortgage Application Denial and 
#                                     the Payment-to-Income Ratio",
#      xlab = "P/I ratio",
#      ylab = "Deny",
#      pch = 20,
#      ylim = c(-0.4, 1.4),
#      cex.main = 0.8)

# add horizontal dashed lines and text
# abline(h = 1, lty = 2, col = "darkred")
# abline(h = 0, lty = 2, col = "darkred")
# text(2.5, 0.9, cex = 0.8, "Mortgage denied")
# text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
# abline(denymod1, 
#        lwd = 1.8, 
#        col = "steelblue")
ggplot(HMDA, aes(x = pirat, y = deny)) +
    geom_point() +
    geom_abline(intercept = coef(denymod1)[1], 
                slope = coef(denymod1)[2],
                color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
    annotate("text", x = 2.5, y = 0.9, label = "Mortgage denied") +
    annotate("text", x = 2.5, y = -0.1, label = "Mortgage approved") +
    labs(title = "Scatterplot Mortgage Application Denial and 
                                         the Payment-to-Income Ratio",
         x = "P/I ratio",
         y = "Deny") +
    ylim(-0.4, 1.4)


# print robust coefficient summary
coeftest(denymod1, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.079910   0.031967 -2.4998   0.01249 *  
#> pirat        0.603535   0.098483  6.1283 1.036e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.090514   0.033430 -2.7076  0.006826 ** 
#> pirat        0.559195   0.103671  5.3939 7.575e-08 ***
#> blackyes     0.177428   0.025055  7.0815 1.871e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 11.2 Probit and Logit Regression ---------------------------------------------

# estimate the simple probit model
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")
#> 
#> z test of coefficients:
#> 
#>             Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -2.19415    0.18901 -11.6087 < 2.2e-16 ***
#> pirat        2.96787    0.53698   5.5269 3.259e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# plot data
# plot(x = HMDA$pirat, 
#      y = HMDA$deny,
#      main = "Probit Model of the Probability of Denial, Given P/I Ratio",
#      xlab = "P/I ratio",
#      ylab = "Deny",
#      pch = 20,
#      ylim = c(-0.4, 1.4),
#      cex.main = 0.85)

# add horizontal dashed lines and text
# abline(h = 1, lty = 2, col = "darkred")
# abline(h = 0, lty = 2, col = "darkred")
# text(2.5, 0.9, cex = 0.8, "Mortgage denied")
# text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
# x <- seq(0, 3, 0.01)
# y <- predict(denyprobit, list(pirat = x), type = "response")

# lines(x, y, lwd = 1.5, col = "steelblue")
ggplot(HMDA, aes(x = pirat, y = deny)) +
    geom_point() +
    geom_smooth(method = "glm", 
                method.args = list(family = binomial(link = "probit")),
                se = FALSE,
                color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
    annotate("text", x = 2.5, y = 0.9, label = "Mortgage denied") +
    annotate("text", x = 2.5, y = -0.1, label = "Mortgage approved") +
    labs(title = "Probit Model of the Probability of Denial, Given P/I Ratio",
         x = "P/I ratio",
         y = "Deny") +
    ylim(-0.4, 1.4)


# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions)
#>          2 
#> 0.06081433


#As a remainder, we have renamed the variable "afam" to "black" in Chapter 11.1
#colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"
#We continue by using an augmented
denyprobit2 <- glm(deny ~ pirat + black, 
                   family = binomial(link = "probit"), 
                   data = HMDA)

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")
#> 
#> z test of coefficients:
#> 
#>              Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -2.258787   0.176608 -12.7898 < 2.2e-16 ***
#> pirat        2.741779   0.497673   5.5092 3.605e-08 ***
#> blackyes     0.708155   0.083091   8.5227 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)
#>         2 
#> 0.1578117


denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")
#> 
#> z test of coefficients:
#> 
#>             Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -4.02843    0.35898 -11.2218 < 2.2e-16 ***
#> pirat        5.88450    1.00015   5.8836 4.014e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# plot data
# plot(x = HMDA$pirat, 
#      y = HMDA$deny,
#      main = "Probit and Logit Models of the Probability of Denial, Given P/I Ratio",
#      xlab = "P/I ratio",
#      ylab = "Deny",
#      pch = 20,
#      ylim = c(-0.4, 1.4),
#      cex.main = 0.9)

# add horizontal dashed lines and text
# abline(h = 1, lty = 2, col = "darkred")
# abline(h = 0, lty = 2, col = "darkred")
# text(2.5, 0.9, cex = 0.8, "Mortgage denied")
# text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
# x <- seq(0, 3, 0.01)
# y_probit <- predict(denyprobit, list(pirat = x), type = "response")
# y_logit <- predict(denylogit, list(pirat = x), type = "response")

# lines(x, y_probit, lwd = 1.5, col = "steelblue")
# lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
# legend("topleft",
#        horiz = TRUE,
#        legend = c("Probit", "Logit"),
#        col = c("steelblue", "black"), 
#        lty = c(1, 2))
ggplot(HMDA, aes(x = pirat, y = deny)) +
    geom_point() +
    geom_smooth(aes(color = "Probit", linetype = "Probit"),
                method = "glm", 
                method.args = list(family = binomial(link = "probit")),
                se = FALSE) +
    geom_smooth(aes(color = "Logit", linetype = "Logit"),
                method = "glm", 
                method.args = list(family = binomial(link = "logit")),
                se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
    annotate("text", x = 2.5, y = 0.9, label = "Mortgage denied") +
    annotate("text", x = 2.5, y = -0.1, label = "Mortgage approved") +
    labs(title = "Probit and Logit Models of the Probability of Denial, Given P/I Ratio",
         x = "P/I ratio",
         y = "Deny") +
    ylim(-0.4, 1.4) +
    scale_color_manual(name = NULL,
                       values = c("Probit" = "steelblue", "Logit" = "black")) +
    scale_linetype_manual(name = NULL,
                          values = c("Probit" = "solid", "Logit" = "dashed")) +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1), 
          legend.direction = "horizontal")


# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")
#> 
#> z test of coefficients:
#> 
#>             Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -4.12556    0.34597 -11.9245 < 2.2e-16 ***
#> pirat        5.37036    0.96376   5.5723 2.514e-08 ***
#> blackyes     1.27278    0.14616   8.7081 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

predictions
#>          1          2 
#> 0.07485143 0.22414592

# 2. Compute difference in probabilities
diff(predictions)
#>         2 
#> 0.1492945


# 11.3 Estimation and Inference in the Logit and Probit Models -----------------

# compute pseudo-R2 for the probit model of mortgage denial
pseudoR2 <- 1 - (denyprobit2$deviance) / (denyprobit2$null.deviance)
pseudoR2
#> [1] 0.08594259


# compute the null model
denyprobit_null <- glm(formula = deny ~ 1, 
                       family = binomial(link = "probit"), 
                       data = HMDA)

# compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1]
#> [1] 0.08594259


# 11.4 Application to the Boston HMDA Data -------------------------------------

# Mean P/I ratio
mean(HMDA$pirat)
#> [1] 0.3308136

# inhouse expense-to-total-income ratio
mean(HMDA$hirat)
#> [1] 0.2553461

# loan-to-value ratio
mean(HMDA$lvrat)
#> [1] 0.7377759

# consumer credit score
mean(as.numeric(HMDA$chist))
#> [1] 2.116387

# mortgage credit score
mean(as.numeric(HMDA$mhist))
#> [1] 1.721008

# public bad credit record
mean(as.numeric(HMDA$phist)-1)
#> [1] 0.07352941

# denied mortgage insurance
prop.table(table(HMDA$insurance))
#> 
#>         no        yes 
#> 0.97983193 0.02016807

# self-employed
prop.table(table(HMDA$selfemp))
#> 
#>        no       yes 
#> 0.8836134 0.1163866

# single
prop.table(table(HMDA$single))
#> 
#>        no       yes 
#> 0.6067227 0.3932773

# high school diploma
prop.table(table(HMDA$hschool))
#> 
#>         no        yes 
#> 0.01638655 0.98361345

# unemployment rate
mean(HMDA$unemp)
#> [1] 3.774496

# condominium
prop.table(table(HMDA$condomin))
#> 
#>        no       yes 
#> 0.7117647 0.2882353

# black
prop.table(table(HMDA$black))
#> 
#>       no      yes 
#> 0.857563 0.142437

# deny
prop.table(table(HMDA$deny))
#> 
#>         0         1 
#> 0.8802521 0.1197479


# define low, medium and high loan-to-value ratio
HMDA$lvrat <- factor(
    ifelse(HMDA$lvrat < 0.8, "low",
           ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")),
    levels = c("low", "medium", "high"))

# convert credit scores to numeric
HMDA$mhist <- as.numeric(HMDA$mhist)
HMDA$chist <- as.numeric(HMDA$chist)


# estimate all 6 models for the denial probability
lpm_HMDA <- lm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
               + insurance + selfemp, data = HMDA)

logit_HMDA <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                  + insurance + selfemp, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

probit_HMDA_1 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_2 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_3 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist 
                     + phist + insurance + selfemp + single + hschool + unemp
                     +condomin + I(mhist==3) + I(mhist==4) + I(chist==3) 
                     + I(chist==4) + I(chist==5)+ I(chist==6), 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_4 <- glm(deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)


rob_se <- list(sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))),
               sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1"))))

stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1, 
          probit_HMDA_2, probit_HMDA_3, probit_HMDA_4,  
          digits = 3,
          # type = "latex", 
          header = FALSE,
          se = rob_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
          type = "html", out = "output/table_CH11_1.html")


# comppute regressor values for an average black person
new <- data.frame(
    "pirat" = mean(HMDA$pirat),
    "hirat" = mean(HMDA$hirat),
    "lvrat" = "low",
    "chist" = mean(HMDA$chist),
    "mhist" = mean(HMDA$mhist),
    "phist" = "no",
    "insurance" = "no",
    "selfemp" = "no",
    "black" = c("no", "yes"),
    "single" = "no",
    "hschool" = "yes",
    "unemp" = mean(HMDA$unemp),
    "condomin" = "no")

# differnce predicted by the LPM
predictions <- predict(lpm_HMDA, newdata = new)
diff(predictions)
#>          2 
#> 0.08369674

# differnce predicted by the logit model
predictions <- predict(logit_HMDA, newdata = new, type = "response")
diff(predictions)
#>          2 
#> 0.04042135

# difference predicted by probit model (3)
predictions <- predict(probit_HMDA_1, newdata = new, type = "response")
diff(predictions)
#>          2 
#> 0.05049716

# difference predicted by probit model (4)
predictions <- predict(probit_HMDA_2, newdata = new, type = "response")
diff(predictions)
#>          2 
#> 0.03978918

# difference predicted by probit model (5)
predictions <- predict(probit_HMDA_3, newdata = new, type = "response")
diff(predictions)
#>          2 
#> 0.04972468

# difference predicted by probit model (6)
predictions <- predict(probit_HMDA_4, newdata = new, type = "response")
diff(predictions)
#>          2 
#> 0.03955893


linearHypothesis(probit_HMDA_4,
                 test = "F",
                 c("blackyes:pirat=0", "blackyes:hirat=0"),
                 vcov = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> blackyes:pirat = 0
#> blackyes:hirat = 0
#> 
#> Model 1: restricted model
#> Model 2: deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist + 
#>     insurance + selfemp + single + hschool + unemp
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1   2366                 
#> 2   2364  2 0.2473 0.7809


linearHypothesis(probit_HMDA_4,
                 test = "F",
                 c("blackyes=0", "blackyes:pirat=0", "blackyes:hirat=0"),
                 vcov = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> blackyes = 0
#> blackyes:pirat = 0
#> blackyes:hirat = 0
#> 
#> Model 1: restricted model
#> Model 2: deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist + 
#>     insurance + selfemp + single + hschool + unemp
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1   2367                      
#> 2   2364  3 4.7774 0.002534 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 11.5 Exercises ---------------------------------------------------------------

# 1. Titanic Survival Data
# attach the dataset `Titanic`
Titanic_1 <- Titanic
# Titanic_1 <- as_tibble(Titanic)

# get an overview over the data
summary(Titanic_1)
# or
str(Titanic_1)
# or
head(Titanic_1)

# print the data the console
Titanic_1

# plot a mosaic
mosaicplot(Titanic_1, main = "Survival on the Titanic")


# 2. Titanic Survival Data - Ctd.
# attach the packages `dplyr` and `readr`
# library(readr)
# library(dplyr)

# use `read_csv()` to import the dataset and assign the data to `Titanic_2`
# Titanic_2 <- read_csv("https://stanford.io/2O9RUCF")
Titanic_2 <- read_csv("data/Exercises_CH11/titanic.csv")

# get an overview over the data and drop `Name`
summary(Titanic_2)
# or
str(Titanic_2)
# or
head(Titanic_2)

Titanic_2 <- Titanic_2[, -3]

# change the column names
colnames(Titanic_2) <- c("Survived", "Class", "Sex", "Age", "Siblings", "Parents", "Fare")

# attach the package `corrplot`
# library(corrplot)

# check correlations using `corrplot()`
corrplot(cor(select_if(Titanic_2, is.numeric)))
# (the highest correlation is between fare and passenger class)


# 3. Titanic Survival Data - Survival Rates
# generate `t_abs`, a contingency table of `Survived` and `Class`
t_abs <- table(Titanic_2$Survived, Titanic_2$Class)

# generate `t_rel`, the table of the relative frequencies
t_rel <- t_abs/nrow(Titanic_2)

# create the barplot
barplot(table(Titanic_2$Survived, Titanic_2$Class)/nrow(Titanic_2),
        col = c("darkred","darkgreen"),
        ylim = c(0,0.6),
        main = "Relative Frequencies of Survival",
        xlab = "Class")
ggplot(Titanic_2) +
    geom_bar(aes(x = Class, y = after_stat(count/sum(count)), 
                 fill = factor(Survived, levels = c(1, 0)))) +
    scale_fill_manual(name = "Survived",
                      values = c("darkred", "darkgreen")) +
    labs(title = "Relative Frequencies of Survival",
         x = "Class", y = NULL) +
    ylim(0, 0.6) +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# 5. Titanic Survival Data — A Linear Probability Model for Survival I
# attach the `AER` package
# library(AER)

# encode `Class` as a factor
Titanic_2$Class <- as.factor(Titanic_2$Class)

# fit the linear probability model, assign it to `surv_mod`
surv_mod <- lm(Survived ~ Class, data = Titanic_2)

# obtain a robust summary of the model coefficients
coeftest(surv_mod, vcovHC)
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.629630   0.033010  19.074  < 2e-16 ***
#> Class2      -0.156804   0.049590  -3.162  0.00162 ** 
#> Class3      -0.385276   0.038346 -10.047  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# predict the probability of survival for all passenger classes
predict(surv_mod, newdata = data.frame("Class" = as.factor(1:3)))
#>         1         2         3 
#> 0.6296296 0.4728261 0.2443532 


# 6. Titanic Survival Data - A Linear Probability Model for Survival II
# compute the class specific estimates   
surv_prob_c1 <- surv_mod$coefficients[1]  
surv_prob_c2 <- surv_prob_c1 + surv_mod$coefficients[2]  
surv_prob_c3 <- surv_prob_c1 + surv_mod$coefficients[3]  

# fit the linear probability model, assign it to `surv_mod`
LPM_mod <- lm(Survived ~ ., data = Titanic_2)

# obtain a robust summary of the model coefficients
coeftest(LPM_mod, vcovHC)
#> t test of coefficients:
#> 
#>                Estimate  Std. Error  t value  Pr(>|t|)    
#> (Intercept)  1.14897720  0.05771825  19.9067 < 2.2e-16 ***
#> Class2      -0.17454604  0.04483443  -3.8931 0.0001065 ***
#> Class3      -0.35824271  0.04540211  -7.8904 8.929e-15 ***
#> Sexmale     -0.50753080  0.03014992 -16.8336 < 2.2e-16 ***
#> Age         -0.00618427  0.00108174  -5.7170 1.485e-08 ***
#> Siblings    -0.05024630  0.01213239  -4.1415 3.783e-05 ***
#> Parents     -0.01942544  0.01947431  -0.9975 0.3188009    
#> Fare         0.00041656  0.00033039   1.2608 0.2077018    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 7. Titanic Survival Data - Logistic Regression
# fit the Logit model and assign it to `Logit_mod`
Logit_mod <- glm(Survived ~ ., 
                 family = binomial(link = "logit"), 
                 data = Titanic_2)

# obtain a robust summary of the model coefficients
coeftest(Logit_mod, vcovHC)
#> z test of coefficients:
#> 
#>               Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept)  4.1097773  0.4618578   8.8984 < 2.2e-16 ***
#> Class2      -1.1614906  0.2760107  -4.2081 2.575e-05 ***
#> Class3      -2.3500224  0.3098841  -7.5836 3.362e-14 ***
#> Sexmale     -2.7567103  0.1971364 -13.9838 < 2.2e-16 ***
#> Age         -0.0434102  0.0082767  -5.2449 1.564e-07 ***
#> Siblings    -0.4015716  0.1041230  -3.8567 0.0001149 ***
#> Parents     -0.1068835  0.1142875  -0.9352 0.3496766    
#> Fare         0.0028229  0.0022390   1.2608 0.2073908    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

passengers <- read_csv("data/Exercises_CH11/passengers.csv",
                       col_types = cols(Class = col_factor()))
    # mutate(Class = as.factor(Class))
# predict the probability of survival for the hypothecial individuals
predict(Logit_mod, newdata = passengers, type = "response")
#>          1          2          3 
#> 0.47825307 0.22295415 0.08038962 


# 8. Titanic Survival Data - Probit Regression
# fit the Probit model, assign it to `Probit_mod`
Probit_mod <- glm(Survived ~ ., 
                  family = binomial(link = "probit"), 
                  data = Titanic_2)

# obtain a robust summary of the model coefficients
coeftest(Probit_mod, vcovHC)
#> z test of coefficients:
#> 
#>               Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept)  2.3646470  0.2614851   9.0431 < 2.2e-16 ***
#> Class2      -0.6629013  0.1601625  -4.1389 3.489e-05 ***
#> Class3      -1.3186829  0.1758669  -7.4982 6.471e-14 ***
#> Sexmale     -1.6250032  0.1093154 -14.8653 < 2.2e-16 ***
#> Age         -0.0245111  0.0047938  -5.1131 3.169e-07 ***
#> Siblings    -0.2306630  0.0574980  -4.0117 6.029e-05 ***
#> Parents     -0.0750691  0.0669832  -1.1207    0.2624    
#> Fare         0.0018100  0.0012832   1.4106    0.1584    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# predict the probability of survival for the hypothecial individuals
predict(Probit_mod, newdata = passengers, type = "response")
#>          1          2          3 
#> 0.47041805 0.23052458 0.08182472 