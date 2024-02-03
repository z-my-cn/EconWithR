# Econometrics with R
# Chapter 12 Instrumental Variables Regression ---------------------------------
library(AER)
library(stargazer)
library(tidyverse)
library(ggplot2)


# 12.1 The IV Estimator with a Single Regressor and a Single Instrument --------

# load the data set and get an overview
# library(AER)
data("CigarettesSW")
CigarettesSW <- as_tibble(CigarettesSW)
summary(CigarettesSW)
#>      state      year         cpi          population           packs       
#>  AL     : 2   1985:48   Min.   :1.076   Min.   :  478447   Min.   : 49.27  
#>  AR     : 2   1995:48   1st Qu.:1.076   1st Qu.: 1622606   1st Qu.: 92.45  
#>  AZ     : 2             Median :1.300   Median : 3697472   Median :110.16  
#>  CA     : 2             Mean   :1.300   Mean   : 5168866   Mean   :109.18  
#>  CO     : 2             3rd Qu.:1.524   3rd Qu.: 5901500   3rd Qu.:123.52  
#>  CT     : 2             Max.   :1.524   Max.   :31493524   Max.   :197.99  
#>  (Other):84                                                                
#>      income               tax            price             taxs       
#>  Min.   :  6887097   Min.   :18.00   Min.   : 84.97   Min.   : 21.27  
#>  1st Qu.: 25520384   1st Qu.:31.00   1st Qu.:102.71   1st Qu.: 34.77  
#>  Median : 61661644   Median :37.00   Median :137.72   Median : 41.05  
#>  Mean   : 99878736   Mean   :42.68   Mean   :143.45   Mean   : 48.33  
#>  3rd Qu.:127313964   3rd Qu.:50.88   3rd Qu.:176.15   3rd Qu.: 59.48  
#>  Max.   :771470144   Max.   :99.00   Max.   :240.85   Max.   :112.63  
#> 


# compute real per capita prices
# CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

#  compute the sales tax
# CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

CigarettesSW <- CigarettesSW %>%
  mutate(rprice = price / cpi,
         salestax = (taxs - tax) / cpi)

# check the correlation between sales tax and price
cor(CigarettesSW$salestax, CigarettesSW$price)
#> [1] 0.6141228

# generate a subset for the year 1995
# c1995 <- subset(CigarettesSW, year == "1995")
c1995 <- CigarettesSW %>%
  filter(year == "1995")


# perform the first stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)

coeftest(cig_s1, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 4.6165463  0.0289177 159.6444 < 2.2e-16 ***
#> salestax    0.0307289  0.0048354   6.3549 8.489e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared
#> [1] 0.4709961


# store the predicted values
lcigp_pred <- cig_s1$fitted.values


# run the stage 2 regression
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)  9.71988    1.70304  5.7074 7.932e-07 ***
#> lcigp_pred  -1.08359    0.35563 -3.0469  0.003822 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# perform TSLS using 'ivreg()'
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)  9.71988    1.52832  6.3598 8.346e-08 ***
#> log(rprice) -1.08359    0.31892 -3.3977  0.001411 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 12.2 The General IV Regression Model -----------------------------------------

# add rincome to the dataset
# CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
CigarettesSW <- CigarettesSW %>%
  mutate(rincome = income / population / cpi)

# c1995 <- subset(CigarettesSW, year == "1995")
c1995 <- CigarettesSW %>%
  filter(year == "1995")

# estimate the model
cig_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + 
                        salestax, data = c1995)

coeftest(cig_ivreg2, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)   9.43066    1.25939  7.4883 1.935e-09 ***
#> log(rprice)  -1.14338    0.37230 -3.0711  0.003611 ** 
#> log(rincome)  0.21452    0.31175  0.6881  0.494917    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# add cigtax to the data set
# CigarettesSW$cigtax <- with(CigarettesSW, tax/cpi)
CigarettesSW <- CigarettesSW %>%
  mutate(cigtax = tax / cpi)

# c1995 <- subset(CigarettesSW, year == "1995")
c1995 <- CigarettesSW %>%
  filter(year == "1995")


# estimate the model
cig_ivreg3 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | 
                        log(rincome) + salestax + cigtax, data = c1995)

coeftest(cig_ivreg3, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)   9.89496    0.95922 10.3157 1.947e-13 ***
#> log(rprice)  -1.27742    0.24961 -5.1177 6.211e-06 ***
#> log(rincome)  0.28040    0.25389  1.1044    0.2753    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 12.3 Checking Instrument Validity --------------------------------------------


# 12.4 Application to the Demand for Cigarettes --------------------------------

# subset data for year 1985
# c1985 <- subset(CigarettesSW, year == "1985")
c1985 <- CigarettesSW %>%
  filter(year == "1985")

# define differences in variables
packsdiff <- log(c1995$packs) - log(c1985$packs)

pricediff <- log(c1995$price/c1995$cpi) - log(c1985$price/c1985$cpi)

incomediff <- log(c1995$income/c1995$population/c1995$cpi) -
    log(c1985$income/c1985$population/c1985$cpi)

salestaxdiff <- (c1995$taxs - c1995$tax)/c1995$cpi - 
    (c1985$taxs - c1985$tax)/c1985$cpi

cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi


# estimate the three models
cig_ivreg_diff1 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                             salestaxdiff)

cig_ivreg_diff2 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                             cigtaxdiff)

cig_ivreg_diff3 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                             salestaxdiff + cigtaxdiff)


# robust coefficient summary for 1.
coeftest(cig_ivreg_diff1, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.117962   0.068217 -1.7292   0.09062 .  
#> pricediff   -0.938014   0.207502 -4.5205 4.454e-05 ***
#> incomediff   0.525970   0.339494  1.5493   0.12832    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# robust coefficient summary for 2.
coeftest(cig_ivreg_diff2, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.017049   0.067217 -0.2536    0.8009    
#> pricediff   -1.342515   0.228661 -5.8712 4.848e-07 ***
#> incomediff   0.428146   0.298718  1.4333    0.1587    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# robust coefficient summary for 3.
coeftest(cig_ivreg_diff3, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.052003   0.062488 -0.8322    0.4097    
#> pricediff   -1.202403   0.196943 -6.1053 2.178e-07 ***
#> incomediff   0.462030   0.309341  1.4936    0.1423    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))

# generate table
stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
          header = FALSE, 
          type = "html", out = "output/table_CH12_1.html",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
          se = rob_se)


# first-stage regressions
mod_relevance1 <- lm(pricediff ~ salestaxdiff + incomediff)
mod_relevance2 <- lm(pricediff ~ cigtaxdiff + incomediff)
mod_relevance3 <- lm(pricediff ~ incomediff + salestaxdiff + cigtaxdiff)


# check instrument relevance for model (1)
linearHypothesis(mod_relevance1, 
                 "salestaxdiff = 0", 
                 vcov = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> salestaxdiff = 0
#> 
#> Model 1: restricted model
#> Model 2: pricediff ~ salestaxdiff + incomediff
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1     46                        
#> 2     45  1 28.445 3.009e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# check instrument relevance for model (2)
linearHypothesis(mod_relevance2, 
                 "cigtaxdiff = 0", 
                 vcov = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> cigtaxdiff = 0
#> 
#> Model 1: restricted model
#> Model 2: pricediff ~ cigtaxdiff + incomediff
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)    
#> 1     46                       
#> 2     45  1 98.034 7.09e-13 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# check instrument relevance for model (3)
linearHypothesis(mod_relevance3, 
                 c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                 vcov = vcovHC, type = "HC1")
#> Linear hypothesis test
#> 
#> Hypothesis:
#> salestaxdiff = 0
#> cigtaxdiff = 0
#> 
#> Model 1: restricted model
#> Model 2: pricediff ~ incomediff + salestaxdiff + cigtaxdiff
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1     46                        
#> 2     44  2 76.916 4.339e-15 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute the J-statistic
cig_iv_OR <- lm(residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff)

cig_OR_test <- linearHypothesis(cig_iv_OR, 
                                c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                                test = "Chisq")
cig_OR_test
#> Linear hypothesis test
#> 
#> Hypothesis:
#> salestaxdiff = 0
#> cigtaxdiff = 0
#> 
#> Model 1: restricted model
#> Model 2: residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff
#> 
#>   Res.Df     RSS Df Sum of Sq Chisq Pr(>Chisq)  
#> 1     46 0.37472                                
#> 2     44 0.33695  2  0.037769 4.932    0.08492 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute correct p-value for J-statistic
pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)
#> [1] 0.02636406


# 12.5 Where Do Valid Instruments Come From? -----------------------------------


# 12.6 Exercises ---------------------------------------------------------------

# 1. The College Distance Data
# attach the package `AER`
# library(AER)           

# load the `CollegeDistance` data set
data("CollegeDistance")
CollegeDistance <- as_tibble(CollegeDistance)

# get an overview over the data set
summary(CollegeDistance)
# or
str(CollegeDistance)
# or
head(CollegeDistance)

# generate a histogram of `distance`
# hist(CollegeDistance$distance, 
#      main = "Distance to College", 
#      xlab = "Distance in 10 Miles")
ggplot(CollegeDistance, aes(x = distance)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Distance in 10 Miles", y = "Frequency", 
       title = "Distance to College")


# 2. The Selection Problem
# regress the log of `wage` on `education` and save the result to `wage_mod_1`
wage_mod_1 <- lm(log(wage) ~ education, data = CollegeDistance)           

# regress log of `wage` on `education` and controls and save the result to `wage_mod_2`
wage_mod_2 <- lm(log(wage) ~ unemp + ethnicity + gender + urban + education, data = CollegeDistance)     

# obtain robust coefficient summaries on both models
coeftest(wage_mod_1, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>              Estimate Std. Error  t value Pr(>|t|)    
#> (Intercept) 2.2132204  0.0163766 135.1457  < 2e-16 ***
#> education   0.0020244  0.0011726   1.7263  0.08435 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(wage_mod_2, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>                      Estimate  Std. Error  t value Pr(>|t|)    
#> (Intercept)        2.15199991  0.01702729 126.3854  < 2e-16 ***
#> unemp              0.01359382  0.00073636  18.4609  < 2e-16 ***
#> ethnicityafam     -0.06191387  0.00610281 -10.1451  < 2e-16 ***
#> ethnicityhispanic -0.05352044  0.00466023 -11.4845  < 2e-16 ***
#> genderfemale      -0.00911503  0.00397011  -2.2959  0.02172 *  
#> urbanyes           0.00893926  0.00465696   1.9195  0.05497 .  
#> education          0.00067227  0.00111645   0.6021  0.54710    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 3. Instrumental Variables Regression Approaches - I
# compute the correlation
cor(CollegeDistance$distance, CollegeDistance$education)
#> [1] -0.09318309

# perform the first stage regression and compute the fraction of explained variation
R2 <- summary(lm(education ~ distance, data = CollegeDistance))$r.squared  
R2
#> [1] 0.008683088

# estimate the IV regression of `log(wage)` on `education` using distance as the instrument and save the result to `wage_mod_iv1`
wage_mod_iv1 <- ivreg(log(wage) ~ education | distance, data = CollegeDistance)           

# perform TSLS regression of `log(wage)` on `education` and controls using distance as the instrument and save the result to `wage_mod_iv2`
wage_mod_iv2 <- ivreg(log(wage) ~ unemp + ethnicity + gender + urban + education | . - education + distance, data = CollegeDistance) 

# obtain robust coefficient summaries on both models
coeftest(wage_mod_iv1, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 2.2212812  0.1603013 13.8569   <2e-16 ***
#> education   0.0014406  0.0116057  0.1241   0.9012    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


coeftest(wage_mod_iv2, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#> Estimate  Std. Error t value  Pr(>|t|)    
#> (Intercept)        1.21717874  0.20081988  6.0610 1.457e-09 ***
#> unemp              0.01422341  0.00096321 14.7667 < 2.2e-16 ***
#> ethnicityafam     -0.02776208  0.01013238 -2.7399  0.006168 ** 
#> ethnicityhispanic -0.03350430  0.00808265 -4.1452 3.454e-05 ***
#> genderfemale      -0.00761014  0.00526501 -1.4454  0.148407    
#> urbanyes           0.00644937  0.00633675  1.0178  0.308838    
#> education          0.06732419  0.01432793  4.6988 2.691e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 4. Instrumental Variables Regression Approaches - II 
# complete the function `TSLS()`
TSLS <- function(Y, X, W = NULL, Z, data) {
    # first stage regression & fitted values
    fs_model <- lm(as.formula(paste(X, "~", paste(c(Z, W), collapse = "+"))), 
                   data = data)
    X_fitted <- fs_model$fitted.values
    
    # second-stage regression
    ss_model <- lm(as.formula(paste(Y, "~", paste(W, collapse = "+"), "+ X_fitted")),  
                   data = data)
    
    # return coefficients of second stage
    return(
        coefficients(ss_model)
    )}

# use `TSLS()` to reproduce the estimates from Exercise 3
TSLS(Y = "log(wage)", X = "education", Z = "distance", data = CollegeDistance)

TSLS(Y = "log(wage)", 
     X = "education", 
     W = c("unemp", "ethnicity", "gender", "urban"), 
     Z = "distance", 
     data = CollegeDistance)


# 5. Should we trust the Results?
