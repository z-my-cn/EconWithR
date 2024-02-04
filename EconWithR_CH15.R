# Econometrics with R
# Chapter 15 Estimation of Dynamic Causal Effects ------------------------------
library(AER)
library(quantmod)
library(dynlm)
library(orcutt)
library(nlme)
library(stargazer)


# 15.1 The Orange Juice Data ---------------------------------------------------

# load the frozen orange juice data set
data("FrozenJuice")

# compute the price index for frozen concentrated juice
FOJCPI <- FrozenJuice[, "price"]/FrozenJuice[, "ppi"]
FOJC_pctc <- 100 * diff(log(FOJCPI))
FDD <- FrozenJuice[, "fdd"]


# convert series to xts objects
FOJCPI_xts <- as.xts(FOJCPI)
FDD_xts <- as.xts(FrozenJuice[, 3])

# Plot orange juice price index
plot(as.zoo(FOJCPI),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Price index", 
     main = "Frozen Concentrated Orange Juice")


# divide plotting area
par(mfrow = c(2, 1))

# Plot percentage changes in prices
plot(as.zoo(FOJC_pctc),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Percent",
     cex.main=0.8,
     main = "Monthly Changes in the Price of Frozen Conentrated Orange Juice")

# plot freezing degree days
plot(as.zoo(FDD),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Freezing degree days",
     cex.main=0.8,
     main = "Monthly Freezing Degree Days in Orlando, FL")


# simple regression of percentage changes on freezing degree days
orange_SR <- dynlm(FOJC_pctc ~ FDD)
coeftest(orange_SR, vcov. = vcovHAC)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -0.42095    0.18683 -2.2531 0.0246064 *  
#> FDD          0.46724    0.13385  3.4906 0.0005167 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# distributed lag model with 6 lags of freezing degree days
orange_DLM <- dynlm(FOJC_pctc ~ FDD + L(FDD, 1:6))
coeftest(orange_DLM, vcov. = vcovHAC)
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)  -0.692961   0.212445 -3.2618 0.0011700 ** 
#> FDD           0.471433   0.135195  3.4871 0.0005242 ***
#> L(FDD, 1:6)1  0.145021   0.081557  1.7782 0.0758853 .  
#> L(FDD, 1:6)2  0.058364   0.058911  0.9907 0.3222318    
#> L(FDD, 1:6)3  0.074166   0.047143  1.5732 0.1162007    
#> L(FDD, 1:6)4  0.036304   0.029335  1.2376 0.2163670    
#> L(FDD, 1:6)5  0.048756   0.031370  1.5543 0.1206535    
#> L(FDD, 1:6)6  0.050246   0.045129  1.1134 0.2659919    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 15.2 Dynamic Causal Effects --------------------------------------------------


# 15.3 Dynamic Multipliers and Cumulative Dynamic Multipliers ------------------

# compute cumulative multipliers
cum_mult <-cumsum(orange_DLM$coefficients[-1])

# rename entries
names(cum_mult) <- paste(0:6, sep = "-", "period CDM")

cum_mult
#> 0-period CDM 1-period CDM 2-period CDM 3-period CDM 4-period CDM 5-period CDM 
#>    0.4714329    0.6164542    0.6748177    0.7489835    0.7852874    0.8340436 
#> 6-period CDM 
#>    0.8842895


# estimate cumulative dynamic multipliers using the modified regression
cum_mult_reg <-dynlm(FOJC_pctc ~ d(FDD) + d(L(FDD,1:5)) + L(FDD,6))
coef(cum_mult_reg)[-1]
#>          d(FDD) d(L(FDD, 1:5))1 d(L(FDD, 1:5))2 d(L(FDD, 1:5))3 d(L(FDD, 1:5))4 
#>       0.4714329       0.6164542       0.6748177       0.7489835       0.7852874 
#> d(L(FDD, 1:5))5       L(FDD, 6) 
#>       0.8340436       0.8842895


# obtain coefficient summary that reports HAC standard errors
coeftest(cum_mult_reg, vcov. = vcovHAC)
#> 
#> t test of coefficients:
#> 
#>                 Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)     -0.69296    0.23668 -2.9278 0.0035431 ** 
#> d(FDD)           0.47143    0.13583  3.4709 0.0005562 ***
#> d(L(FDD, 1:5))1  0.61645    0.13145  4.6896 3.395e-06 ***
#> d(L(FDD, 1:5))2  0.67482    0.16009  4.2151 2.882e-05 ***
#> d(L(FDD, 1:5))3  0.74898    0.17263  4.3387 1.682e-05 ***
#> d(L(FDD, 1:5))4  0.78529    0.17351  4.5260 7.255e-06 ***
#> d(L(FDD, 1:5))5  0.83404    0.18236  4.5737 5.827e-06 ***
#> L(FDD, 6)        0.88429    0.19303  4.5810 5.634e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 15.4 HAC Standard Errors -----------------------------------------------------

# function that computes rho tilde
acf_c <- function(x, j) {
    return(
        t(x[-c(1:j)]) %*% na.omit(Lag(x, j)) / t(x) %*% x
    )
}

# simulate time series with serially correlated errors
set.seed(1)

N <- 100

eps <- arima.sim(n = N, model = list(ma = 0.5))
X <- runif(N, 1, 10)
Y <- 0.5 * X + eps

# compute OLS residuals
res <- lm(Y ~ X)$res

# compute v
v <- (X - mean(X)) * res

# compute robust estimate of beta_1 variance
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * res^2) ) / 
    (1/N * sum((X - mean(X))^2))^2

# rule of thumb truncation parameter
m <- floor(0.75 * N^(1/3))

# compute correction factor
f_hat_T <- 1 + 2 * sum(
    (m - 1:(m-1))/m * sapply(1:(m - 1), function(i) acf_c(x = v, j = i))
) 

# compute Newey-West HAC estimate of the standard error 
sqrt(var_beta_hat * f_hat_T)
#> [1] 0.04036208


# Using NeweyWest():
NW_VCOV <- NeweyWest(lm(Y ~ X), 
                     lag = m - 1, prewhite = F, 
                     adjust = T)

# compute standard error
sqrt(diag(NW_VCOV))[2]
#>          X 
#> 0.04036208


example_mod <- lm(Y ~ X)
coeftest(example_mod, vcov = NW_VCOV)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.542310   0.235423  2.3036  0.02336 *  
#> X           0.423305   0.040362 10.4877  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 15.5 Estimation of Dynamic Causal Effects with Strictly Exogeneous Regressors ----

# set seed for reproducibility
set.seed(1)

# simulate a time series with serially correlated errors
obs <- 501
eps <- arima.sim(n = obs-1 , model = list(ar = 0.5))
X <- arima.sim(n = obs, model = list(ar = 0.25))
Y <- 0.1 * X[-1] + 0.25 * X[-obs] + eps
X <- ts(X[-1])

# estimate the distributed lag model
dlm <- dynlm(Y ~ X + L(X))

# par(mfrow = c(1, 1))
# check that the residuals are serially correlated
acf(residuals(dlm))


# coefficient summary using the Newey-West SE estimates
coeftest(dlm, vcov = NeweyWest, prewhite = F, adjust = T)
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 0.038340   0.073411  0.5223  0.601717    
#> X           0.123661   0.046710  2.6474  0.008368 ** 
#> L(X)        0.247406   0.046377  5.3347 1.458e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# estimate the ADL(2,1) representation of the distributed lag model
adl21_dynamic <- dynlm(Y ~ L(Y) + X + L(X, 1:2))

# plot the sample autocorrelaltions of residuals
acf(adl21_dynamic$residuals)


# compute estimated dynamic effects using coefficient restrictions
# in the ADL(2,1) representation
t <- adl21_dynamic$coefficients

c("hat_beta_1" = t[3],
  "hat_beta_2" = t[4] + t[3] * t[2])
#>          hat_beta_1.X hat_beta_2.L(X, 1:2)1 
#>             0.1176425             0.2478484


# GLS: estimate quasi-differenced specification by OLS
iGLS_dynamic <- dynlm(I(Y- 0.5 * L(Y)) ~ I(X - 0.5 * L(X)) + I(L(X) - 0.5 * L(X, 2)))

summary(iGLS_dynamic)
#> 
#> Time series regression with "ts" data:
#> Start = 3, End = 500
#> 
#> Call:
#> dynlm(formula = I(Y - 0.5 * L(Y)) ~ I(X - 0.5 * L(X)) + I(L(X) - 
#>     0.5 * L(X, 2)))
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.0325 -0.6375 -0.0499  0.6658  3.7724 
#> 
#> Coefficients:
#>                         Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)              0.01620    0.04564   0.355  0.72273    
#> I(X - 0.5 * L(X))        0.12000    0.04237   2.832  0.00481 ** 
#> I(L(X) - 0.5 * L(X, 2))  0.25266    0.04237   5.963 4.72e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.017 on 495 degrees of freedom
#> Multiple R-squared:  0.07035,    Adjusted R-squared:  0.0666 
#> F-statistic: 18.73 on 2 and 495 DF,  p-value: 1.442e-08


X_t <- c(X[-1])
# create first lag
X_l1 <- c(X[-500])
Y_t <- c(Y[-1])

# iterated cochrane-orcutt procedure
summary(cochrane.orcutt(lm(Y_t ~ X_t + X_l1)))
#> Call:
#> lm(formula = Y_t ~ X_t + X_l1)
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 0.032885   0.085163   0.386   0.69956    
#> X_t         0.120128   0.042534   2.824   0.00493 ** 
#> X_l1        0.252406   0.042538   5.934 5.572e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.0165 on 495 degrees of freedom
#> Multiple R-squared:  0.0704 ,  Adjusted R-squared:  0.0666
#> F-statistic: 18.7 on 2 and 495 DF,  p-value: < 1.429e-08
#> 
#> Durbin-Watson statistic 
#> (original):    1.06907 , p-value: 1.05e-25
#> (transformed): 1.98192 , p-value: 4.246e-01


# feasible GLS maximum likelihood estimation procedure
summary(gls(Y_t ~ X_t + X_l1, correlation = corAR1()))
#> Generalized least squares fit by REML
#>   Model: Y_t ~ X_t + X_l1 
#>   Data: NULL 
#>        AIC     BIC    logLik
#>   1451.847 1472.88 -720.9235
#> 
#> Correlation Structure: AR(1)
#>  Formula: ~1 
#>  Parameter estimate(s):
#>       Phi 
#> 0.4668343 
#> 
#> Coefficients:
#>                  Value  Std.Error  t-value p-value
#> (Intercept) 0.03929124 0.08530544 0.460595  0.6453
#> X_t         0.11986994 0.04252270 2.818963  0.0050
#> X_l1        0.25287471 0.04252497 5.946500  0.0000
#> 
#>  Correlation: 
#>      (Intr) X_t  
#> X_t  0.039       
#> X_l1 0.037  0.230
#> 
#> Standardized residuals:
#>         Min          Q1         Med          Q3         Max 
#> -3.00075518 -0.64255522 -0.05400347  0.69101814  3.28555793 
#> 
#> Residual standard error: 1.14952 
#> Degrees of freedom: 499 total; 496 residual


# 15.6 Orange Juice Prices and Cold Weather ------------------------------------

# estimate distributed lag models of frozen orange juice price changes
FOJC_mod_DM <- dynlm(FOJC_pctc ~ L(FDD, 0:18))
FOJC_mod_CM1 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18))
FOJC_mod_CM2 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))


# set lag orders as regressor labels 
attr(FOJC_mod_DM$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))
attr(FOJC_mod_CM1$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))
attr(FOJC_mod_CM2$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))


length(FDD)
#> [1] 612


# gather HAC standard error errors in a list
SEs <- list(sqrt(diag(NeweyWest(FOJC_mod_DM, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 14, prewhite = F))),
            sqrt(diag(NeweyWest(FOJC_mod_CM2, lag = 7, prewhite = F))))


stargazer(FOJC_mod_DM , FOJC_mod_CM1, FOJC_mod_CM1, FOJC_mod_CM2,
          title = "Dynamic Effects of a Freezing Degree Day on the Price of Orange Juice",
          header = FALSE, 
          digits = 3, 
          column.labels = c("Dynamic Multipliers", rep("Dynamic Cumulative Multipliers", 3)),
          dep.var.caption  = "Dependent Variable: Monthly Percentage Change in Orange Juice Price",
          dep.var.labels.include = FALSE,
          covariate.labels = as.character(0:18),
          omit = "season",
          se = SEs,
          no.space = T,
          add.lines = list(c("Monthly indicators?","no", "no", "no", "yes"),
                           c("HAC truncation","7", "7", "14", "7")),
          omit.stat = c("rsq", "f","ser"),
          type = "html", out = "output/table_CH15_1.html")


# estimates on mothly dummies
FOJC_mod_CM2$coefficients[-c(1:20)]
#> season(FDD)Feb season(FDD)Mar season(FDD)Apr season(FDD)May season(FDD)Jun 
#>     -0.9565759     -0.6358007      0.5006770     -1.0801764      0.3195624 
#> season(FDD)Jul season(FDD)Aug season(FDD)Sep season(FDD)Oct season(FDD)Nov 
#>      0.1951113      0.3644312     -0.4130969     -0.1566622      0.3116534 
#> season(FDD)Dec 
#>      0.1481589


# test if coefficients on monthly dummies are zero
unres_model <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))

res_model <- update(unres_model, formula = . ~ . - season(FDD))

waldtest(unres_model, 
         res_model, 
         vcov = NeweyWest(unres_model, lag = 7, prewhite = F))
#> Wald test
#> 
#> Model 1: FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD)
#> Model 2: FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18)
#>   Res.Df  Df      F Pr(>F)
#> 1    563                  
#> 2    574 -11 0.9683 0.4743


# 95% CI bounds
point_estimates <- FOJC_mod_DM$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[1]],
                   "upper" = point_estimates + 1.96 * SEs[[1]])[-1, ]

# plot the estimated dynamic multipliers
plot(0:18, point_estimates[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue", 
     ylim = c(-0.4, 1),
     xlab = "Lag",
     ylab = "Dynamic multiplier",
     main = "Dynamic Effect of FDD on Orange Juice Price")

# add a dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[,1], col = "darkred")
lines(0:18, CI_bounds[,2], col = "darkred")


# 95% CI bounds
point_estimates <- FOJC_mod_CM1$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[2]],
                   "upper" = point_estimates + 1.96 * SEs[[2]])[-1,]


# plot estimated dynamic multipliers
plot(0:18, point_estimates[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue", 
     ylim = c(-0.4, 1.6),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect of FDD on Orange Juice Price",
     cex.main=0.8)

# add dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[, 1], col = "darkred")
lines(0:18, CI_bounds[, 2], col = "darkred")


# estimate cumulative multiplieres using different sample periods
FOJC_mod_CM1950 <- update(FOJC_mod_CM1, start = c(1950, 1), end = c(1966, 12))

FOJC_mod_CM1967 <- update(FOJC_mod_CM1, start = c(1967, 1), end = c(1983, 12))

FOJC_mod_CM1984 <- update(FOJC_mod_CM1, start = c(1984, 1), end = c(2000, 12))


# plot estimated dynamic cumulative multipliers (1950-1966)
plot(0:18, FOJC_mod_CM1950$coefficients[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue",
     xlim = c(0, 20),
     ylim = c(-0.5, 2),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect for Different Sample Periods")

# plot estimated dynamic multipliers (1967-1983)
lines(0:18, FOJC_mod_CM1967$coefficients[-1], lwd = 2)

# plot estimated dynamic multipliers (1984-2000)
lines(0:18, FOJC_mod_CM1984$coefficients[-1], lwd = 2, col = "darkgreen")

# add dashed line at 0
abline(h = 0, lty = 2)

# add annotations
text(18, -0.24, "1984 - 2000")
text(18, 0.6, "1967 - 1983")
text(18, 1.2, "1950 - 1966")


# set up a range of possible break dates
tau <- c(window(time(FDD), 
                time(FDD)[round(612/100*15)], 
                time(FDD)[round(612/100*85)]))

# initialize the vector of F-statistics
Fstats <- numeric(length(tau))

# the restricted model
res_model <- dynlm(FOJC_pctc ~ L(FDD, 0:18))

# estimation, loop over break dates
for(i in 1:length(tau)) {
    
    # set up dummy variable
    D <- time(FOJC_pctc) > tau[i]
    
    # estimate DL model with intercations
    unres_model <- dynlm(FOJC_pctc ~ D * L(FDD, 0:18))
    
    # compute and save F-statistic
    Fstats[i] <- waldtest(res_model, 
                          unres_model, 
                          vcov = NeweyWest(unres_model, lag = 7, prewhite = F))$F[2]
    
}


# QLR test statistic
max(Fstats)
#> [1] 36.76819
