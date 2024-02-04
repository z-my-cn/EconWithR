# Econometrics with R
# Chapter 16 Additional Topics in Time Series Regression -----------------------
library(AER)
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)


# 16.1 Vector Autoregressions --------------------------------------------------

# load the U.S. macroeconomic data set
USMacroSWQ <- read_xlsx("data/us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

# Change the name of the first column
colnames(USMacroSWQ)[1] <- "Date"


# format the date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")

# define GDP as ts object
GDP <- ts(USMacroSWQ$GDPC96,
          start = c(1957, 1), 
          end = c(2013, 4), 
          frequency = 4)

# define GDP growth as a ts object
GDPGrowth <- ts(400*log(GDP[-1]/GDP[-length(GDP)]),
                start = c(1957, 2), 
                end = c(2013, 4), 
                frequency = 4)

# 3-months Treasury bill interest rate as a 'ts' object
TB3MS <- ts(USMacroSWQ$TB3MS,
            start = c(1957, 1), 
            end = c(2013, 4), 
            frequency = 4)

# 10-years Treasury bonds interest rate as a 'ts' object
TB10YS <- ts(USMacroSWQ$GS10, 
             start = c(1957, 1), 
             end = c(2013, 4), 
             frequency = 4)

# generate the term spread series
TSpread <- TB10YS - TB3MS


# Estimate both equations using 'dynlm()'
VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2), 
                 start = c(1981, 1), 
                 end = c(2012, 4))

VAR_EQ2 <- dynlm(TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2),
                 start = c(1981, 1),
                 end = c(2012, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "TSpread_t-1", "TSpread_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> Intercept    0.516344   0.524429  0.9846 0.3267616    
#> Growth_t-1   0.289553   0.110827  2.6127 0.0101038 *  
#> Growth_t-2   0.216392   0.085879  2.5197 0.0130255 *  
#> TSpread_t-1 -0.902549   0.358290 -2.5190 0.0130498 *  
#> TSpread_t-2  1.329831   0.392660  3.3867 0.0009503 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(VAR_EQ2, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error t value  Pr(>|t|)    
#> Intercept    0.4557740  0.1214227  3.7536 0.0002674 ***
#> Growth_t-1   0.0099785  0.0218424  0.4568 0.6485920    
#> Growth_t-2  -0.0572451  0.0264099 -2.1676 0.0321186 *  
#> TSpread_t-1  1.0582279  0.0983750 10.7571 < 2.2e-16 ***
#> TSpread_t-2 -0.2191902  0.1086198 -2.0180 0.0457712 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, TSpread), start = c(1980, 3), end = c(2012, 4))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est
#> 
#> VAR Estimation Results:
#> ======================= 
#> 
#> Estimated coefficients for equation GDPGrowth: 
#> ============================================== 
#> Call:
#> GDPGrowth = GDPGrowth.l1 + TSpread.l1 + GDPGrowth.l2 + TSpread.l2 + const 
#> 
#> GDPGrowth.l1   TSpread.l1 GDPGrowth.l2   TSpread.l2        const 
#>    0.2895533   -0.9025493    0.2163919    1.3298305    0.5163440 
#> 
#> 
#> Estimated coefficients for equation TSpread: 
#> ============================================ 
#> Call:
#> TSpread = GDPGrowth.l1 + TSpread.l1 + GDPGrowth.l2 + TSpread.l2 + const 
#> 
#> GDPGrowth.l1   TSpread.l1 GDPGrowth.l2   TSpread.l2        const 
#>  0.009978489  1.058227945 -0.057245123 -0.219190243  0.455773969


# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$GDPGrowth)$adj.r.squared
#> [1] 0.2887223
summary(VAR_est$varresult$TSpread)$adj.r.squared
#> [1] 0.8254311


# Granger causality tests:

# test if term spread has no power in explaining GDP growth
linearHypothesis(VAR_EQ1, 
                 hypothesis.matrix = c("TSpread_t-1", "TSpread_t-2"),
                 vcov. = sandwich)
#> Linear hypothesis test
#> 
#> Hypothesis:
#> TSpread_t-1
#> TSpread_t-2
#> 
#> Model 1: restricted model
#> Model 2: GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F   Pr(>F)   
#> 1    125                      
#> 2    123  2 5.9094 0.003544 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# test if GDP growth has no power in explaining term spread
linearHypothesis(VAR_EQ2, 
                 hypothesis.matrix = c("Growth_t-1", "Growth_t-2"),
                 vcov. = sandwich)
#> Linear hypothesis test
#> 
#> Hypothesis:
#> Growth_t-1
#> Growth_t-2
#> 
#> Model 1: restricted model
#> Model 2: TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    125                    
#> 2    123  2 3.4777 0.03395 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute iterated forecasts for GDP growth and term spread for the next 10 quarters
forecasts <- predict(VAR_est)
forecasts
#> $GDPGrowth
#>           fcst     lower    upper       CI
#>  [1,] 1.738653 -3.006124 6.483430 4.744777
#>  [2,] 1.692193 -3.312731 6.697118 5.004925
#>  [3,] 1.911852 -3.282880 7.106583 5.194731
#>  [4,] 2.137070 -3.164247 7.438386 5.301317
#>  [5,] 2.329667 -3.041435 7.700769 5.371102
#>  [6,] 2.496815 -2.931819 7.925449 5.428634
#>  [7,] 2.631849 -2.846390 8.110088 5.478239
#>  [8,] 2.734819 -2.785426 8.255064 5.520245
#>  [9,] 2.808291 -2.745597 8.362180 5.553889
#> [10,] 2.856169 -2.722905 8.435243 5.579074
#> 
#> $TSpread
#>           fcst        lower    upper        CI
#>  [1,] 1.676746  0.708471226 2.645021 0.9682751
#>  [2,] 1.884098  0.471880228 3.296316 1.4122179
#>  [3,] 1.999409  0.336348101 3.662470 1.6630609
#>  [4,] 2.080836  0.242407507 3.919265 1.8384285
#>  [5,] 2.131402  0.175797245 4.087008 1.9556052
#>  [6,] 2.156094  0.125220562 4.186968 2.0308738
#>  [7,] 2.161783  0.085037834 4.238528 2.0767452
#>  [8,] 2.154170  0.051061544 4.257278 2.1031082
#>  [9,] 2.138164  0.020749780 4.255578 2.1174139
#> [10,] 2.117733 -0.007139213 4.242605 2.1248722


# visualize the iterated forecasts
plot(forecasts)


# estimate models for direct two-quarter-ahead forecasts
VAR_EQ1_direct <- dynlm(GDPGrowth ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

VAR_EQ2_direct <- dynlm(TSpread ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

# compute direct two-quarter-ahead forecasts
coef(VAR_EQ1_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))
#>          [,1]
#> [1,] 2.439497

coef(VAR_EQ2_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))
#>         [,1]
#> [1,] 1.66578


# 16.2 Orders of Integration and the DF-GLS Unit Root Test ---------------------

# define ts object of the U.S. PCE Price Index
PCECTPI <- ts(log(USMacroSWQ$PCECTPI), 
              start = c(1957, 1), 
              end = c(2012, 4), 
              freq = 4)

# plot logarithm of the PCE Price Index
plot(PCECTPI,
     main = "Log of United States PCE Price Index",
     ylab = "Logarithm",
     col = "steelblue", 
     lwd = 2)


# plot U.S. Inflation Rate
plot(400 * diff(PCECTPI),
     main = "United States Inflation Rate",
     ylab = "Percent per annum",
     col = "steelblue", 
     lwd = 2)

# add a dashed line at y =  0 
abline(0, 0, lty = 2)


# DF-GLS test for unit root in GDP
summary(ur.ers(log(window(GDP, start = c(1962, 1), end = c(2012, 4))),
               model = "trend", 
               lag.max = 2))
#> 
#> ############################################### 
#> # Elliot, Rothenberg and Stock Unit Root Test # 
#> ############################################### 
#> 
#> Test of type DF-GLS 
#> detrending of series with intercept and trend 
#> 
#> 
#> Call:
#> lm(formula = dfgls.form, data = data.dfgls)
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.025739 -0.004054  0.000017  0.004619  0.033620 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> yd.lag       -0.01213    0.01012  -1.199  0.23207    
#> yd.diff.lag1  0.28583    0.07002   4.082 6.47e-05 ***
#> yd.diff.lag2  0.19320    0.07058   2.737  0.00676 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.007807 on 198 degrees of freedom
#> Multiple R-squared:  0.1504, Adjusted R-squared:  0.1376 
#> F-statistic: 11.69 on 3 and 198 DF,  p-value: 4.392e-07
#> 
#> 
#> Value of test-statistic is: -1.1987 
#> 
#> Critical values of DF-GLS are:
#>                  1pct  5pct 10pct
#> critical values -3.48 -2.89 -2.57


# 16.3 Cointegration -----------------------------------------------------------

# reproduce Figure 16.2 of the book

# plot both interest series
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates")

# add the term spread series
lines(as.zoo(TSpread), 
      col = "steelblue",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")

# shade the term spread
polygon(c(time(TB3MS), rev(time(TB3MS))), 
        c(TB10YS, rev(TB3MS)),
        col = alpha("steelblue", alpha = 0.3),
        border = NA)

# add horizontal line at 0
abline(0, 0)

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS", "Term Spread"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))


# test for nonstationarity of 3-month treasury bills using ADF test
ur.df(window(TB3MS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -2.1004 2.2385

# test for nonstationarity of 10-years treasury bonds using ADF test
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -1.0079 0.5501

# test for nonstationarity of 3-month treasury bills using DF-GLS test
ur.ers(window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
#> 
#> ############################################################### 
#> # Elliot, Rothenberg and Stock Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -1.8042

# test for nonstationarity of 10-years treasury bonds using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
#> 
#> ############################################################### 
#> # Elliot, Rothenberg and Stock Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -0.942


# test if term spread is stationary (cointegration of interest rates) using ADF
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1), c(2012 ,4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -3.9308 7.7362

# test if term spread is stationary (cointegration of interest rates) using DF-GLS
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1),c(2012, 4)),
       model = "constant", 
       lag.max = 6)
#> 
#> ############################################################### 
#> # Elliot, Rothenberg and Stock Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -3.8576


# estimate first-stage regression of EG-ADF test
FS_EGADF <- dynlm(window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, c(1962, 1),
                                                                  c(2012, 4)))
FS_EGADF
#> 
#> Time series regression with "ts" data:
#> Start = 1962(1), End = 2012(4)
#> 
#> Call:
#> dynlm(formula = window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, 
#>     c(1962, 1), c(2012, 4)))
#> 
#> Coefficients:
#>                           (Intercept)  window(TB3MS, c(1962, 1), c(2012, 4))  
#>                                2.4642                                 0.8147


# estimate first-stage regression of EG-ADF test
FS_EGADF <- dynlm(window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, c(1962, 1),
                                                                  c(2012, 4)))
FS_EGADF
#> 
#> Time series regression with "ts" data:
#> Start = 1962(1), End = 2012(4)
#> 
#> Call:
#> dynlm(formula = window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, 
#>     c(1962, 1), c(2012, 4)))
#> 
#> Coefficients:
#>                           (Intercept)  window(TB3MS, c(1962, 1), c(2012, 4))  
#>                                2.4642                                 0.8147


# compute the residuals
z_hat <- resid(FS_EGADF)

# compute the ADF test statistic
ur.df(z_hat, lags = 6, type = "none", selectlags = "AIC")
#> 
#> ############################################################### 
#> # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
#> ############################################################### 
#> 
#> The value of the test statistic is: -3.1935


TB10YS <- window(TB10YS, c(1962, 1), c(2012 ,4))
TB3MS <- window(TB3MS, c(1962, 1), c(2012, 4))

# set up error correction term
VECM_ECT <- TB10YS - TB3MS

# estimate both equations of the VECM using 'dynlm()'
VECM_EQ1 <- dynlm(d(TB10YS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))
VECM_EQ2 <- dynlm(d(TB3MS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))

# rename regressors for better readability
names(VECM_EQ1$coefficients) <- c("Intercept", "D_TB3MS_l1", "D_TB3MS_l2",
                                  "D_TB10YS_l1", "D_TB10YS_l2", "ect_l1")
names(VECM_EQ2$coefficients) <- names(VECM_EQ1$coefficients)

# coefficient summaries using HAC standard errors
coeftest(VECM_EQ1, vcov. = NeweyWest(VECM_EQ1, prewhite = F, adjust = T))
#> 
#> t test of coefficients:
#> 
#>               Estimate Std. Error t value Pr(>|t|)   
#> Intercept    0.1227089  0.0551419  2.2253 0.027205 * 
#> D_TB3MS_l1  -0.0016601  0.0727060 -0.0228 0.981807   
#> D_TB3MS_l2  -0.0680845  0.0435059 -1.5649 0.119216   
#> D_TB10YS_l1  0.2264878  0.0957071  2.3665 0.018939 * 
#> D_TB10YS_l2 -0.0734486  0.0703476 -1.0441 0.297740   
#> ect_l1      -0.0878871  0.0285644 -3.0768 0.002393 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(VECM_EQ2, vcov. = NeweyWest(VECM_EQ2, prewhite = F, adjust = T))
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value Pr(>|t|)  
#> Intercept   -0.060746   0.107937 -0.5628  0.57422  
#> D_TB3MS_l1   0.240003   0.111611  2.1504  0.03276 *
#> D_TB3MS_l2  -0.155883   0.153845 -1.0132  0.31220  
#> D_TB10YS_l1  0.113740   0.125571  0.9058  0.36617  
#> D_TB10YS_l2 -0.147519   0.112630 -1.3098  0.19182  
#> ect_l1       0.031506   0.050519  0.6236  0.53359  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 16.4 Volatility Clustering and Autoregressive Conditional Heteroskedasticity ----

# import data on the Wilshire 5000 index
W5000 <- read.csv2("data/Wilshire5000.csv", 
                   stringsAsFactors = F, 
                   header = T, 
                   sep = ",", 
                   na.strings = ".")

# transform the columns
W5000$DATE <- as.Date(W5000$DATE)
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)

# remove NAs
W5000 <- na.omit(W5000)

# compute daily percentage changes
W5000_PC <- data.frame("Date" = W5000$DATE, 
                       "Value" = as.numeric(Delt(W5000$WILL5000INDFC) * 100))
W5000_PC <- na.omit(W5000_PC)


# plot percentage changes
plot(W5000_PC, 
     ylab = "Percent", 
     main = "Daily Percentage Changes",
     type=  "l", 
     col =  "steelblue", 
     lwd =  0.5)

# add horizontal line at y = 0
abline(0, 0)


# plot sample autocorrelation of daily percentage changes
acf(W5000_PC$Value, main = "Wilshire 5000 Series")


# estimate GARCH(1,1) model of daily percentage changes
GARCH_Wilshire <- garchFit(data = W5000_PC$Value, trace = F)


# compute deviations of the percentage changes from their mean
dev_mean_W5000_PC <- W5000_PC$Value - GARCH_Wilshire@fit$coef[1]

# plot deviation of percentage changes from mean
plot(W5000_PC$Date, dev_mean_W5000_PC, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     cex.main=0.8,
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)
