# Econometrics with R
# Chapter 09 Assessing Studies Based on Multiple Regression --------------------
library(AER)
library(mvtnorm)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(stargazer)

# 9.1 Internal and External Validity -------------------------------------------


# 9.2 Threats to Internal Validity of Multiple Regression Analysis -------------

# set seed for reproducibility
set.seed(3)

# simulate data set
X <- runif(100, -5, 5)
Y <- X^2 + rnorm(100)

# estimate the regression function
ms_mod <- lm(Y ~ X)
ms_mod
#> 
#> Call:
#> lm(formula = Y ~ X)
#> 
#> Coefficients:
#> (Intercept)            X  
#>     8.11363     -0.04684


# plot the data
# plot(X, Y, 
#      main = "Misspecification of Functional Form",
#      pch = 20,
#      col = "steelblue")

# plot the linear regression line
# abline(ms_mod, 
#        col = "red",
#        lwd = 2)
# legend("bottomright",
#        bg = "transparent",
#        cex = 0.8,
#        lwd = 2,
#        col ="red", 
#        legend = "Linear Regression Line")
ggplot() +
  geom_point(aes(x = X, y = Y), 
             col = "steelblue") +
  geom_smooth(aes(x = X, y = Y, color = "Linear Regression Line"),
              method = "lm", se = FALSE,) +
  scale_color_manual(name = NULL,
                     values = c("Linear Regression Line" = "red")) +
  labs(title = "Misspecification of Functional Form") +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))


# set seed
set.seed(1)

# load the package 'mvtnorm' and simulate bivariate normal data
# library(mvtnorm)
# dat <- data.frame(
#     rmvnorm(1000, c(50, 100), 
#             sigma = cbind(c(10, 5), c(5, 10))))
dat <- rmvnorm(1000, c(50, 100), 
               sigma = cbind(c(10, 5), c(5, 10)))

# set columns names
colnames(dat) <- c("X", "Y")
dat <- as_tibble(dat)

# estimate the model (without measurement error)
noerror_mod <- lm(Y ~ X, data = dat)

# estimate the model (with measurement error in X)
dat$X <- dat$X + rnorm(n = 1000, sd = sqrt(10))
error_mod <- lm(Y ~ X, data = dat)

# print estimated coefficients to console
noerror_mod$coefficients
#> (Intercept)           X 
#>  76.3002047   0.4755264
error_mod$coefficients
#> (Intercept)           X 
#>   87.276004    0.255212


# plot sample data
# plot(dat$X, dat$Y, 
#      pch = 20, 
#      col = "steelblue",
#      xlab = "X",
#      ylab = "Y")

# add population regression function
# abline(coef = c(75, 0.5), 
#        col = "darkgreen",
#        lwd  = 1.5)

# add estimated regression functions
# abline(noerror_mod, 
#        col = "purple",
#        lwd  = 1.5)

# abline(error_mod, 
#        col = "darkred",
#        lwd  = 1.5)

# add legend
# legend("topleft",
#        bg = "transparent",
#        cex = 0.8,
#        lty = 1,
#        col = c("darkgreen", "purple", "darkred"), 
#        legend = c("Population", "No Errors", "Errors"))
ggplot(dat, aes(x = X, y = Y)) +
  geom_point(col = "steelblue") +
  geom_abline(aes(intercept = 75, slope = 0.5, color = "Population")) +
  geom_abline(aes(intercept = noerror_mod$coefficients[1], 
                  slope = noerror_mod$coefficients[2], 
                  color = "No Errors")) +
  geom_abline(aes(intercept = error_mod$coefficients[1],
                  slope = error_mod$coefficients[2],
                  color = "Errors")) +
  scale_color_manual(name = NULL,
                     values = c("Population" = "darkgreen",
                                "No Errors" = "purple", 
                                "Errors" = "darkred")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# set seed
set.seed(1)

# simulate data
# dat <- data.frame(
#     rmvnorm(1000, c(50, 100), 
#             sigma = cbind(c(10, 5), c(5, 10))))
dat <- rmvnorm(1000, c(50, 100), 
               sigma = cbind(c(10, 5), c(5, 10)))

colnames(dat) <- c("X", "Y")
dat <- as_tibble(dat)

# mark 500 randomly selected observations
id <- sample(1:1000, size = 500)

# plot(dat$X[-id], 
#      dat$Y[-id], 
#      col = "steelblue", 
#      pch = 20,
#      cex = 0.8,
#      xlab = "X",
#      ylab = "Y")

# points(dat$X[id], 
#        dat$Y[id],
#        cex = 0.8,
#        col = "gray", 
#        pch = 20)

# add the population regression function
# abline(coef = c(75, 0.5), 
#        col = "darkgreen",
#        lwd  = 1.5)

# add the estimated regression function for the full sample
# abline(noerror_mod)

# estimate model case 1 and add the regression line
# dat <- dat[-id, ]

# c1_mod <- lm(dat$Y ~ dat$X, data = dat)
c1_mod <- lm(Y ~ X, data = dat[-id,])
# abline(c1_mod, col = "purple")

# add a legend
# legend("bottomright",
#        lty = 1,
#        bg = "transparent",
#        cex = 0.8,
#        col = c("darkgreen", "black", "purple"), 
#        legend = c("Population", "Full sample", "500 obs. randomly selected"))
ggplot(dat, aes(x = X, y = Y)) +
  geom_point(data = dat[-id,], col = "steelblue") +
  geom_point(data = dat[id,], col = "gray") +
  geom_abline(aes(intercept = 75, slope = 0.5, color = "Population")) +
  geom_abline(aes(intercept = noerror_mod$coefficients[1], 
                  slope = noerror_mod$coefficients[2], 
                  color = "Full sample")) +
  geom_abline(aes(intercept = c1_mod$coefficients[1],
                  slope = c1_mod$coefficients[2],
                  color = "500 obs. randomly selected")) +
  scale_color_manual(name = NULL,
                     limits = c("Population", "Full sample", 
                                "500 obs. randomly selected"),
                     values = c("Population" = "darkgreen",
                                "Full sample" = "black", 
                                "500 obs. randomly selected" = "purple")) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))


# set random seed
set.seed(1)

# simulate data
# dat <- data.frame(
#     rmvnorm(1000, c(50, 100), 
#             sigma = cbind(c(10, 5), c(5, 10))))
dat <- rmvnorm(1000, c(50, 100), 
               sigma = cbind(c(10, 5), c(5, 10)))

colnames(dat) <- c("X", "Y")
dat <- as_tibble(dat)

# mark observations
# id <- dat$X >= 45

# plot(dat$X[-id], 
#      dat$Y[-id], 
#      col = "steelblue",
#      cex = 0.8,
#      pch = 20,
#      xlab = "X",
#      ylab = "Y")

# points(dat$X[id], 
#        dat$Y[id], 
#        col = "gray",
#        cex = 0.8,
#        pch = 20)

# add population regression function
# abline(coef = c(75, 0.5), 
#        col = "darkgreen",
#        lwd  = 1.5)

# add estimated regression function for full sample
# abline(noerror_mod)

# estimate model case 1, add regression line
# id <- which(dat$X >= 45)
# dat <- dat[-id, ]

# c2_mod <- lm(dat$Y ~ dat$X, data = dat)
c2_mod <- lm(Y ~ X, data = filter(dat, X < 45))
# abline(c2_mod, col = "purple")

# add legend
# legend("topleft",
#        lty = 1,
#        bg = "transparent",
#        cex = 0.8,
#        col = c("darkgreen", "black", "purple"), 
#        legend = c("Population", "Full sample",expression(paste("Obs.with ",
#                                                                X <= 45))))
ggplot(dat, aes(x = X, y = Y)) +
  geom_point(data = filter(dat, X < 45), col = "steelblue") +
  geom_point(data = filter(dat, X >= 45), col = "gray") +
  geom_abline(aes(intercept = 75, slope = 0.5, color = "Population")) +
  geom_abline(aes(intercept = noerror_mod$coefficients[1], 
                  slope = noerror_mod$coefficients[2], 
                  color = "Full sample")) +
  geom_abline(aes(intercept = c2_mod$coefficients[1],
                  slope = c2_mod$coefficients[2],
                  color = "Obs. with X <= 45")) +
  scale_color_manual(name = NULL,
                     limits = c("Population", "Full sample", 
                                "Obs. with X <= 45"),
                     values = c("Population" = "darkgreen",
                                "Full sample" = "black", 
                                "Obs. with X <= 45" = "purple"),
                     labels = c("Population", "Full sample",
                                expression(paste("Obs. with ", X <= 45)))) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# set random seed
set.seed(1)

# simulate data
# dat <- data.frame(
#     rmvnorm(1000, c(50,100), 
#             sigma = cbind(c(10,5), c(5,10))))
dat <- rmvnorm(1000, c(50, 100), 
               sigma = cbind(c(10, 5), c(5, 10)))

colnames(dat) <- c("X","Y")
dat <- as_tibble(dat)

# mark observations
# id <- which(dat$X <= 55 & dat$Y >= 100)

# plot(dat$X[-id], 
#      dat$Y[-id], 
#      col = "gray",
#      cex = 0.8,
#      pch = 20,
#      xlab = "X",
#      ylab = "Y")

# points(dat$X[id], 
#        dat$Y[id], 
#        col = "steelblue",
#        cex = 0.8,
#        pch = 20)

# add population regression function
# abline(coef = c(75, 0.5), 
#        col = "darkgreen",
#        lwd  = 1.5)

# add estimated regression function for full sample
# abline(noerror_mod)

# estimate model case 1, add regression line
# dat <- dat[id, ]

# c3_mod <- lm(dat$Y ~ dat$X, data = dat)
c3_mod <- lm(Y ~ X, data = filter(dat, X <= 55 & Y >= 100))
# abline(c3_mod, col = "purple")

# add legend
# legend("bottomright",
#        lty = 1,
#        bg = "transparent",
#        cex = 0.8,
#        col = c("darkgreen", "black", "purple"), 
#        legend = c("Population", "Full sample",expression(paste(X <= 55,"&",
#                                                                Y >= 100))))
ggplot(dat, aes(x = X, y = Y)) +
  geom_point(data = filter(dat, X <= 55 & Y >= 100), col = "steelblue") +
  geom_point(data = filter(dat, !(X <= 55 & Y >= 100)), col = "gray") +
  geom_abline(aes(intercept = 75, slope = 0.5, color = "Population")) +
  geom_abline(aes(intercept = noerror_mod$coefficients[1], 
                  slope = noerror_mod$coefficients[2], 
                  color = "Full sample")) +
  geom_abline(aes(intercept = c3_mod$coefficients[1],
                  slope = c3_mod$coefficients[2],
                  color = "x <= 55 & y >= 100")) +
  scale_color_manual(name = NULL,
                     limits = c("Population", "Full sample", 
                                "x <= 55 & y >= 100"),
                     values = c("Population" = "darkgreen",
                                "Full sample" = "black", 
                                "x <= 55 & y >= 100" = "purple"),
                     labels = c("Population", "Full sample",
                                expression(paste(X <= 55,"&", Y >= 100)))) +
  labs(x = "X", y = "Y") +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))


# load the data set
# library(AER)
data("CigarettesSW")
CigarettesSW <- as_tibble(CigarettesSW)
# c1995 <- subset(CigarettesSW, year == "1995")
c1995 <- CigarettesSW %>% 
    filter(year == "1995")

# estimate the model
cigcon_mod <- lm(log(packs) ~ log(price), data = c1995)
cigcon_mod
#> 
#> Call:
#> lm(formula = log(packs) ~ log(price), data = c1995)
#> 
#> Coefficients:
#> (Intercept)   log(price)  
#>      10.850       -1.213


# plot the estimated regression line and the data
# plot(log(c1995$price), log(c1995$packs),
#      xlab = "ln(Price)",
#      ylab = "ln(Consumption)",
#      main = "Demand for Cigarettes",
#      pch = 20,
#      col = "steelblue")

# abline(cigcon_mod, 
#        col = "darkred", 
#        lwd = 1.5)
# add legend
# legend("topright",lty=1,col= "darkred", "Estimated Regression Line")
ggplot(c1995, aes(x = log(price), y = log(packs))) +
  geom_point(col = "steelblue") +
  geom_smooth(aes(x = log(price), y = log(packs), 
                  color = "Estimated Regression Line"),
              method = "lm", se = FALSE) +
  scale_color_manual(name = NULL,
                     values = c("Estimated Regression Line" = "darkred")) +
  labs(x = "ln(Price)", y = "ln(Consumption)") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))


# 9.3 Internal and External Validity when the Regression is Used for Forecasting ---- 

data("CASchools")
CASchools <- as_tibble(CASchools)

CASchools <- CASchools %>% 
    mutate(STR = students/teachers,
           score = (read + math)/2)
           

linear_model <- lm(score ~ STR, data = CASchools)
linear_model
#> 
#> Call:
#> lm(formula = score ~ STR, data = CASchools)
#> 
#> Coefficients:
#> (Intercept)          STR  
#>      698.93        -2.28


# 9.4 Example: Test Scores and Class Size --------------------------------------

# attach the 'MASchools' dataset
data("MASchools")
MASchools <- as_tibble(MASchools)
summary(MASchools)
#>    district         municipality           expreg       expspecial   
#>  Length:220         Length:220         Min.   :2905   Min.   : 3832  
#>  Class :character   Class :character   1st Qu.:4065   1st Qu.: 7442  
#>  Mode  :character   Mode  :character   Median :4488   Median : 8354  
#>                                        Mean   :4605   Mean   : 8901  
#>                                        3rd Qu.:4972   3rd Qu.: 9722  
#>                                        Max.   :8759   Max.   :53569  
#>                                                                      
#>      expbil           expocc          exptot        scratio      
#>  Min.   :     0   Min.   :    0   Min.   :3465   Min.   : 2.300  
#>  1st Qu.:     0   1st Qu.:    0   1st Qu.:4730   1st Qu.: 6.100  
#>  Median :     0   Median :    0   Median :5155   Median : 7.800  
#>  Mean   :  3037   Mean   : 1104   Mean   :5370   Mean   : 8.107  
#>  3rd Qu.:     0   3rd Qu.:    0   3rd Qu.:5789   3rd Qu.: 9.800  
#>  Max.   :295140   Max.   :15088   Max.   :9868   Max.   :18.400  
#>                                                  NA's   :9       
#>     special          lunch          stratio          income      
#>  Min.   : 8.10   Min.   : 0.40   Min.   :11.40   Min.   : 9.686  
#>  1st Qu.:13.38   1st Qu.: 5.30   1st Qu.:15.80   1st Qu.:15.223  
#>  Median :15.45   Median :10.55   Median :17.10   Median :17.128  
#>  Mean   :15.97   Mean   :15.32   Mean   :17.34   Mean   :18.747  
#>  3rd Qu.:17.93   3rd Qu.:20.02   3rd Qu.:19.02   3rd Qu.:20.376  
#>  Max.   :34.30   Max.   :76.20   Max.   :27.00   Max.   :46.855  
#>                                                                  
#>      score4          score8          salary         english       
#>  Min.   :658.0   Min.   :641.0   Min.   :24.96   Min.   : 0.0000  
#>  1st Qu.:701.0   1st Qu.:685.0   1st Qu.:33.80   1st Qu.: 0.0000  
#>  Median :711.0   Median :698.0   Median :35.88   Median : 0.0000  
#>  Mean   :709.8   Mean   :698.4   Mean   :35.99   Mean   : 1.1177  
#>  3rd Qu.:720.0   3rd Qu.:712.0   3rd Qu.:37.96   3rd Qu.: 0.8859  
#>  Max.   :740.0   Max.   :747.0   Max.   :44.49   Max.   :24.4939  
#>                  NA's   :40      NA's   :25


# Customized variables in MASchools
# MASchools$score <- MASchools$score4 
# MASchools$STR <- MASchools$stratio
MASchools <- MASchools %>% 
    mutate(score = score4,
           STR = stratio)

# Reproduce Table 9.1 of the book
vars <- c("score", "STR", "english", "lunch", "income")

cbind(CA_mean = sapply(CASchools[, vars], mean),
      CA_sd   = sapply(CASchools[, vars], sd),
      MA_mean = sapply(MASchools[, vars], mean),
      MA_sd   = sapply(MASchools[, vars], sd))
#>           CA_mean     CA_sd    MA_mean     MA_sd
#> score   654.15655 19.053347 709.827273 15.126474
#> STR      19.64043  1.891812  17.344091  2.276666
#> english  15.76816 18.285927   1.117676  2.900940
#> lunch    44.70524 27.123381  15.315909 15.060068
#> income   15.31659  7.225890  18.746764  5.807637


# estimate linear model
Linear_model_MA <- lm(score ~ income, data = MASchools)
Linear_model_MA
#> 
#> Call:
#> lm(formula = score ~ income, data = MASchools)
#> 
#> Coefficients:
#> (Intercept)       income  
#>     679.387        1.624

# estimate linear-log model
Linearlog_model_MA <- lm(score ~ log(income), data = MASchools) 
Linearlog_model_MA
#> 
#> Call:
#> lm(formula = score ~ log(income), data = MASchools)
#> 
#> Coefficients:
#> (Intercept)  log(income)  
#>      600.80        37.71

# estimate Cubic model
cubic_model_MA <- lm(score ~ I(income) + I(income^2) + I(income^3), data = MASchools)
cubic_model_MA
#> 
#> Call:
#> lm(formula = score ~ I(income) + I(income^2) + I(income^3), data = MASchools)
#> 
#> Coefficients:
#> (Intercept)    I(income)  I(income^2)  I(income^3)  
#>  600.398531    10.635382    -0.296887     0.002762


# plot data
# plot(MASchools$income, MASchools$score,
#      pch = 20,
#      col = "steelblue",
#      xlab = "District income",
#      ylab = "Test score",
#      xlim = c(0, 50),
#      ylim = c(620, 780))

# add estimated regression line for the linear model
# abline(Linear_model_MA, lwd = 2)

# add estimated regression function for Linear-log model
# order_id  <- order(MASchools$income)

# lines(MASchools$income[order_id],
#       fitted(Linearlog_model_MA)[order_id], 
#       col = "darkgreen", 
#       lwd = 2)

# add estimated cubic regression function
# lines(x = MASchools$income[order_id], 
#       y = fitted(cubic_model_MA)[order_id],
#       col = "orange", 
#       lwd = 2) 

# add a legend
# legend("topleft",
#        legend = c("Linear", "Linear-Log", "Cubic"),
#        lty = 1,
#        col = c("Black", "darkgreen", "orange"))
ggplot(MASchools, aes(x = income, y = score)) +
  geom_point(col = "steelblue") +
  geom_abline(aes(intercept = Linear_model_MA$coefficients[1], 
                  slope = Linear_model_MA$coefficients[2], 
                  color = "Linear")) +
  geom_line(aes(x = income, y = fitted(Linearlog_model_MA), 
                color = "Linear-Log")) +
  geom_line(aes(x = income, y = fitted(cubic_model_MA), 
                color = "Cubic")) +
  scale_color_manual(name = NULL,
                     limits = c("Linear", "Linear-Log", "Cubic"),
                     values = c("Linear" = "black",
                                "Linear-Log" = "darkgreen",
                                "Cubic" = "orange")) +
  labs(x = "District income", y = "Test score") +
  xlim(0, 50) +
  ylim(620, 780) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# add 'HiEL' to 'MASchools'
# MASchools$HiEL <- as.numeric(MASchools$english > median(MASchools$english))
MASchools <- MASchools %>% 
    mutate(HiEL = as.numeric(english > median(english)))

# estimate the model specifications from Table 9.2 of the book
TSMA_mod1 <- lm(score ~ STR, data = MASchools)

TSMA_mod2 <- lm(score ~ STR + english + lunch + log(income), 
                data = MASchools)

TSMA_mod3 <- lm(score ~ STR + english + lunch + income + I(income^2) 
                + I(income^3), data = MASchools)

TSMA_mod4 <- lm(score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income 
                + I(income^2) + I(income^3), data = MASchools)

TSMA_mod5 <- lm(score ~ STR + HiEL + I(income^2) + I(income^3) + HiEL:STR 
                + lunch + income, data = MASchools)

TSMA_mod6 <- lm(score ~ STR + I(income^2) + I(income^3)+ lunch 
                + income, data = MASchools)

# gather robust standard errors
rob_se <- list(sqrt(diag(vcovHC(TSMA_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(TSMA_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(TSMA_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(TSMA_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(TSMA_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(TSMA_mod6, type = "HC1"))))

# generate a table with 'stargazer()'
# library(stargazer)

stargazer(TSMA_mod1, TSMA_mod2, TSMA_mod3, 
          TSMA_mod4, TSMA_mod5, TSMA_mod6,
          title = "Regressions Using Massachusetts Test Score Data",
          # type = "latex",
          digits = 3,
          header = FALSE,
          se = rob_se,
          # object.names = TRUE,
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)"),
          type = "html", out = "output/table_CH09_1.html")


# F-test model (3)
linearHypothesis(TSMA_mod3, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC(TSMA_mod3, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(income^2) = 0
#> I(income^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + english + lunch + income + I(income^2) + I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1    215                        
#> 2    213  2 7.7448 0.0005664 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# F-tests model (4)
linearHypothesis(TSMA_mod4, 
                 c("STR=0", "I(STR^2)=0", "I(STR^3)=0"), 
                 vcov. = vcovHC(TSMA_mod4, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> STR = 0
#> I(STR^2) = 0
#> I(STR^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income + 
#>     I(income^2) + I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    214                    
#> 2    211  3 2.8565 0.03809 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

linearHypothesis(TSMA_mod4, 
                 c("I(STR^2)=0", "I(STR^3)=0"), 
                 vcov. = vcovHC(TSMA_mod4, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(STR^2) = 0
#> I(STR^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income + 
#>     I(income^2) + I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    213                 
#> 2    211  2 0.4463 0.6406

linearHypothesis(TSMA_mod4, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC(TSMA_mod4, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(income^2) = 0
#> I(income^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income + 
#>     I(income^2) + I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F    Pr(>F)    
#> 1    213                        
#> 2    211  2 7.7487 0.0005657 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#F-tests model (5)
linearHypothesis(TSMA_mod5, 
                 c("STR=0", "STR:HiEL=0"), 
                 vcov. = vcovHC(TSMA_mod5, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> STR = 0
#> STR:HiEL = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + HiEL + HiEL:STR + lunch + income + I(income^2) + 
#>     I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)  
#> 1    214                   
#> 2    212  2 4.0062 0.0196 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
linearHypothesis(TSMA_mod5, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC(TSMA_mod5, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(income^2) = 0
#> I(income^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + HiEL + HiEL:STR + lunch + income + I(income^2) + 
#>     I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1    214                      
#> 2    212  2 5.8468 0.003375 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
linearHypothesis(TSMA_mod5, 
                 c("HiEL=0", "STR:HiEL=0"), 
                 vcov. = vcovHC(TSMA_mod5, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> HiEL = 0
#> STR:HiEL = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + HiEL + HiEL:STR + lunch + income + I(income^2) + 
#>     I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F Pr(>F)
#> 1    214                 
#> 2    212  2 1.5835 0.2077

# F-tests model (6)
linearHypothesis(TSMA_mod6, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC(TSMA_mod6, type = "HC1"))
#> Linear hypothesis test
#> 
#> Hypothesis:
#> I(income^2) = 0
#> I(income^3) = 0
#> 
#> Model 1: restricted model
#> Model 2: score ~ STR + lunch + income + I(income^2) + I(income^3)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1    216                      
#> 2    214  2 6.5479 0.001737 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


TSMA_mod3$coefficients[2] / sd(MASchools$score) * (-2)
#>        STR 
#> 0.08474001


TestScore_mod2 <- lm(score ~ STR + english + lunch + log(income), 
                     data = CASchools)
TestScore_mod2$coefficients[2] / sd(CASchools$score) * (-2)
#>        STR 
#> 0.07708103


# 9.5 Exercises ----------------------------------------------------------------

# 1. Simulation Study: Misspecification of Functional Form
# set seed for reproducibility
set.seed(3)

# set number of sampling iterations
N <- 1000

# initialize vector  `beta_hats`
beta_hats <- c()

# loop estimation
for (i in 1:N) {
    
    # simulate the dataset
    X <- runif(100, -5, 5)
    Y <- X^2 + rnorm(100)
    
    # estimate the linear regression function
    ms_mod <- lm(Y ~ X)
    # save the estimate
    beta_hats[i] <- ms_mod$coefficients[1]
}

# compare mean of estimates and the true parameter
mean(beta_hats) == 0


# 2. Simulation Study: Errors-in-Variables Bias
# set seed for reproducibility
set.seed(123)

# attach the package `mvtnorm`
# library(mvtnorm)

# set number of sampling iterations
N <- 1000

# initialize the vector `beta_hats`
beta_hats <- c()

# loop estimation
for (i in 1:N) {
    
    # simulate the dataset and set the column names
    # d <- data.frame(rmvnorm(100, c(50, 100), 
    #                         sigma = cbind(c(10, 5), c(5, 10))))
    d <- rmvnorm(100, c(50, 100), 
                 sigma = cbind(c(10, 5), c(5, 10)))
    colnames(d) <- c("X", "Y")
    d <- as_tibble(d)
    
    # add the measurement error to `X`
    d[,1] <- d[,1] + rnorm(100,0,sqrt(10))
    
    # estimate the simple linear regression model
    ms_mod <- lm(Y ~ X, data = d)
    
    # save the estimate
    beta_hats[i] <- ms_mod$coefficients[2]
}

# compute the sample mean of the estimates
mean(beta_hats)
#> [1] 0.2520756
