# Econometrics with R
# Chapter 14 Introduction to Time Series Regression and Forecasting ------------
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(tidyverse)
library(patchwork)


# 14.1 Using Regression Models for Forecasting ---------------------------------

# library(AER)
data(CASchools)   
CASchools <- as_tibble(CASchools)
# CASchools$STR <- CASchools$students/CASchools$teachers       
# CASchools$score <- (CASchools$read + CASchools$math)/2
CASchools <- CASchools %>% 
  mutate(STR = students/teachers,
         score = (read + math)/2)

mod <- lm(score ~ STR, data = CASchools)
mod
#> 
#> Call:
#> lm(formula = score ~ STR, data = CASchools)
#> 
#> Coefficients:
#> (Intercept)          STR  
#>      698.93        -2.28


predict(mod, newdata = data.frame("STR" = 25))
#>        1 
#> 641.9377


# 14.2 Time Series Data and Serial Correlation ---------------------------------

# attach the package 'quantmod'
# library(quantmod)
# library(readxl)


# load US macroeconomic data
USMacroSWQ <- read_xlsx("data/us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

# format date column
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")

# adjust column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")


# GDP series as xts object
GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]

# GDP growth series as xts object
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))


# reproduce Figure 14.1 (a) of the book
# plot(log(as.zoo(GDP)),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Logarithm",
#      xlab = "Date",
#      main = "U.S. Quarterly Real GDP")
ggplot(data = USMacroSWQ, aes(x = Date, y = log(GDPC96))) +
  geom_line(color = "steelblue", size = 2) +
  labs(x = "Date", y = "Logarithm", title = "U.S. Quarterly Real GDP")


# reproduce Figure 14.1 (b) of the book
# plot(as.zoo(GDPGrowth),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Logarithm",
#      xlab = "Date",
#      main = "U.S. Real GDP Growth Rates")
ggplot() +
  geom_line(aes(x = index(GDPGrowth), y = coredata(GDPGrowth)),
            color = "steelblue", linewidth = 1, na.rm = TRUE) +
  labs(x = "Date", y = "Logarithm", title = "U.S. Real GDP Growth Rates")


# compute logarithms, annual growth rates and 1st lag of growth rates
quants <- function(series) {
    s <- series
    return(
        data.frame("Level" = s,
                   "Logarithm" = log(s),
                   "AnnualGrowthRate" = 400 * log(s / lag(s)),
                   "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
    )
}


# obtain a data.frame with level, logarithm, annual growth rate and its 1st lag of GDP
quants(GDP["2011-07::2013-01"])
#>            Level Logarithm AnnualGrowthRate X1stLagAnnualGrowthRate
#> 2011 Q3 15062.14  9.619940               NA                      NA
#> 2011 Q4 15242.14  9.631819        4.7518062                      NA
#> 2012 Q1 15381.56  9.640925        3.6422231               4.7518062
#> 2012 Q2 15427.67  9.643918        1.1972004               3.6422231
#> 2012 Q3 15533.99  9.650785        2.7470216               1.1972004
#> 2012 Q4 15539.63  9.651149        0.1452808               2.7470216
#> 2013 Q1 15583.95  9.653997        1.1392015               0.1452808


acf(na.omit(GDPGrowth), lag.max = 4, plot = F)
#> 
#> Autocorrelations of series 'na.omit(GDPGrowth)', by lag
#> 
#>  0.00  0.25  0.50  0.75  1.00 
#> 1.000 0.352 0.273 0.114 0.106


# define series as xts objects
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]

DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]

JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP), USMacroSWQ$Date)["1960::2013"]

# attach NYSESW data
data("NYSESW")  
NYSESW <- xts(Delt(NYSESW))


# divide plotting area into 2x2 matrix
# par(mfrow = c(2, 2))

# plot the series
# plot(as.zoo(USUnemp),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Percent",
#      xlab = "Date",
#      main = "US Unemployment Rate",
#      cex.main = 0.8)
p1 <- ggplot(data = USMacroSWQ, aes(x = Date, y = UNRATE)) +
  geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
  labs(x = "Date", y = "Percent", title = "US Unemployment Rate")

# plot(as.zoo(DollarPoundFX),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Dollar per pound",
#      xlab = "Date",
#      main = "U.S. Dollar / B. Pound Exchange Rate",
#      cex.main = 0.8)
p2 <- ggplot(data = USMacroSWQ, aes(x = Date, y = EXUSUK)) +
  geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
  labs(x = "Date", y = "Dollar per pound", 
       title = "U.S. Dollar / B. Pound Exchange Rate")

# plot(as.zoo(JPIndProd),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Logarithm",
#      xlab = "Date",
#      main = "Japanese Industrial Production",
#      cex.main = 0.8)
p3 <- ggplot(data = USMacroSWQ, aes(x = Date, y = log(JAPAN_IP))) +
  geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
  labs(x = "Date", y = "Logarithm", 
       title = "Japanese Industrial Production")
# plot(as.zoo(NYSESW),
#      col = "steelblue",
#      lwd = 2,
#      ylab = "Percent per Day",
#      xlab = "Date",
#      main = "New York Stock Exchange Composite Index",
#      cex.main = 0.8)
p4 <- ggplot(data = NYSESW, aes(x = index(NYSESW), y = coredata(NYSESW))) +
  geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
  labs(x = "Date", y = "Percent per Day", 
       title = "New York Stock Exchange Composite Index")

p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)


# compute sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), plot = F, lag.max = 10)
#> 
#> Autocorrelations of series 'na.omit(NYSESW)', by lag
#> 
#>      0      1      2      3      4      5      6      7      8      9     10 
#>  1.000  0.040 -0.016 -0.023  0.000 -0.036 -0.027 -0.059  0.013  0.017  0.004


# plot sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), main = "Sample Autocorrelation for NYSESW Data")


# 14.3 Autoregressions ---------------------------------------------------------

# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

# estimate the model
ar.ols(GDPGRSub, 
       order.max = 1, 
       demean = F, 
       intercept = T)
#> 
#> Call:
#> ar.ols(x = GDPGRSub, order.max = 1, demean = F, intercept = T)
#> 
#> Coefficients:
#>      1  
#> 0.3384  
#> 
#> Intercept: 1.995 (0.2993) 
#> 
#> Order selected 1  sigma^2 estimated as  9.886


# length of data set
N <-length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

# estimate the model
armod <- lm(GDPGR_level ~ GDPGR_lags)
armod
#> 
#> Call:
#> lm(formula = GDPGR_level ~ GDPGR_lags)
#> 
#> Coefficients:
#> (Intercept)   GDPGR_lags  
#>      1.9950       0.3384


# robust summary
coeftest(armod, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) 1.994986   0.351274  5.6793 4.691e-08 ***
#> GDPGR_lags  0.338436   0.076188  4.4421 1.470e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# library(forecast)

# assign GDP growth rate in 2012:Q4
new <- data.frame("GDPGR_lags" = GDPGR_level[N-1])

# forecast GDP growth rate in 2013:Q1
forecast(armod, newdata = new)
#>   Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#> 1       2.044155 -2.036225 6.124534 -4.213414 8.301723


# compute the forecast error
forecast(armod, newdata = new)$mean - GDPGrowth["2013"][1]
#>                 x
#> 2013 Q1 0.9049532

# R^2
summary(armod)$r.squared
#> [1] 0.1149576

# SER
summary(armod)$sigma
#> [1] 3.15979


# estimate the AR(2) model
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level), 2))

coeftest(GDPGR_AR2, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>                       Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)           1.631747   0.402023  4.0588 7.096e-05 ***
#> L(ts(GDPGR_level))    0.277787   0.079250  3.5052 0.0005643 ***
#> L(ts(GDPGR_level), 2) 0.179269   0.079951  2.2422 0.0260560 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# R^2
summary(GDPGR_AR2)$r.squared
#> [1] 0.1425484

# SER
summary(GDPGR_AR2)$sigma
#> [1] 3.132122


# AR(2) forecast of GDP growth in 2013:Q1 
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1],
                                                GDPGR_level[N-2]))


# compute AR(2) forecast error 
GDPGrowth["2013"][1] - forecast
#>                 x
#> 2013 Q1 -1.025358 


# 14.4 Can You Beat the Market? (Part I) ---------------------------------------

# read in data on stock returns
SReturns <- read_xlsx("data/Stock_Returns_1931_2002.xlsx",
                      sheet = 1,
                      col_types = "numeric")


#Alternatively, ASC  cen be read directly from internet.

# SReturns <- read.csv("https://www.princeton.edu/~mwatson/Stock-Watson_3u/Students/EE_Datasets/Stock_Returns_1931_2002.asc",
#                      sep = "\t", header = FALSE )

colnames(SReturns) <- c( "Year", "Month", "ExReturn", "ln_DivYield" )


# convert to ts object
StockReturns <- ts(SReturns[, 3:4], 
                   start = c(1931, 1), 
                   end = c(2002, 12), 
                   frequency = 12)


# estimate AR models:

# AR(1)
SR_AR1 <- dynlm(ExReturn ~ L(ExReturn), 
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(2)
SR_AR2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2), 
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(4)
SR_AR4 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2:4), 
                data = StockReturns, start = c(1960, 1), end = c(2002, 12))


# compute robust standard errors
rob_se <- list(sqrt(diag(sandwich(SR_AR1))),
               sqrt(diag(sandwich(SR_AR2))),
               sqrt(diag(sandwich(SR_AR4))))


# generate table using 'stargazer()'
stargazer(SR_AR1, SR_AR2, SR_AR4,
          title = "Autoregressive Models of Monthly Excess Stock Returns",
          header = FALSE, 
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("AR(1)", "AR(2)", "AR(4)"),
          dep.var.caption  = "Dependent Variable: Excess Returns on the CSRP Value-Weighted Index",
          dep.var.labels.include = FALSE,
          covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", 
                               "$excess return_{t-3}$", "$excess return_{t-4}$", 
                               "Intercept"),
          se = rob_se,
          omit.stat = "rsq", 
          type = "html", out = "output/table_CH14_1.html")


# 14.5 Additional Predictors and The ADL Model ---------------------------------

# 3-month Treasury bills interest rate
TB3MS <- xts(USMacroSWQ$TB3MS, USMacroSWQ$Date)["1960::2012"]

# 10-year Treasury bonds interest rate
TB10YS <- xts(USMacroSWQ$GS10, USMacroSWQ$Date)["1960::2012"]

# term spread
TSpread <- TB10YS - TB3MS


# reproduce Figure 14.2 (a) of the book
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Interest Rates")

# define function that transform years to class 'yearqtr'
YToYQTR <- function(years) {
    return(
        sort(as.yearqtr(sapply(years, paste, c("Q1", "Q2", "Q3", "Q4"))))
    )
}

# recessions
recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001,
                        2007:2008))

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions), 
        col = alpha("steelblue", alpha = 0.3))

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS"),
       col = c("darkred", "steelblue"),
       lwd = c(2, 2))


# reproduce Figure 14.2 (b) of the book
plot(as.zoo(TSpread), 
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions), 
        col = alpha("steelblue", alpha = 0.3))


# convert growth and spread series to ts objects
GDPGrowth_ts <- ts(GDPGrowth, 
                   start = c(1960, 1), 
                   end = c(2013, 4), 
                   frequency = 4)

TSpread_ts <- ts(TSpread, 
                 start = c(1960, 1), 
                 end = c(2012, 4), 
                 frequency = 4)

# join both ts objects
ADLdata <- ts.union(GDPGrowth_ts, TSpread_ts)


# estimate the ADL(2,1) model of GDP growth
GDPGR_ADL21 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) +
                         L(TSpread_ts), start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL21, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>                    Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)        0.954990   0.486976  1.9611 0.051260 . 
#> L(GDPGrowth_ts)    0.267729   0.082562  3.2428 0.001387 **
#> L(GDPGrowth_ts, 2) 0.192370   0.077683  2.4763 0.014104 * 
#> L(TSpread_ts)      0.444047   0.182637  2.4313 0.015925 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# 2012:Q3 / 2012:Q4 data on GDP growth and term spread
subset <- window(ADLdata, c(2012, 3), c(2012, 4))

# ADL(2,1) GDP growth forecast for 2013:Q1
ADL21_forecast <- coef(GDPGR_ADL21) %*% c(1, subset[2, 1], subset[1, 1], 
                                          subset[2, 2])
ADL21_forecast
#>          [,1]
#> [1,] 2.241689

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL21_forecast
#>           Qtr1
#> 2013 -1.102487


# estimate the ADL(2,2) model of GDP growth
GDPGR_ADL22 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) 
                     + L(TSpread_ts) + L(TSpread_ts, 2), 
                     start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL22, vcov. = sandwich)
#> 
#> t test of coefficients:
#> 
#>                     Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)         0.967967   0.472470  2.0487 0.041800 * 
#> L(GDPGrowth_ts)     0.243175   0.077836  3.1242 0.002049 **
#> L(GDPGrowth_ts, 2)  0.177070   0.077027  2.2988 0.022555 * 
#> L(TSpread_ts)      -0.139554   0.422162 -0.3306 0.741317   
#> L(TSpread_ts, 2)    0.656347   0.429802  1.5271 0.128326   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# ADL(2,2) GDP growth forecast for 2013:Q1
ADL22_forecast <- coef(GDPGR_ADL22) %*% c(1, subset[2, 1], subset[1, 1],
                                          subset[2, 2], subset[1, 2])
ADL22_forecast
#>          [,1]
#> [1,] 2.274407

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL22_forecast
#>           Qtr1
#> 2013 -1.135206


# compare adj. R2
c("Adj.R2 AR(2)" = summary(GDPGR_AR2)$adj.r.squared,
  "Adj.R2 ADL(2,1)" = summary(GDPGR_ADL21)$adj.r.squared,
  "Adj.R2 ADL(2,2)" = summary(GDPGR_ADL22)$adj.r.squared)
#>    Adj.R2 AR(2) Adj.R2 ADL(2,1) Adj.R2 ADL(2,2) 
#>       0.1338873       0.1620156       0.1691531

# compare SER
c("SER AR(2)" = summary(GDPGR_AR2)$sigma,
  "SER ADL(2,1)" = summary(GDPGR_ADL21)$sigma,
  "SER ADL(2,2)" = summary(GDPGR_ADL22)$sigma)
#>    SER AR(2) SER ADL(2,1) SER ADL(2,2) 
#>     3.132122     3.070760     3.057655

# F-test on coefficients of term spread
linearHypothesis(GDPGR_ADL22, 
                 c("L(TSpread_ts)=0", "L(TSpread_ts, 2)=0"),
                 vcov. = sandwich)
#> Linear hypothesis test
#> 
#> Hypothesis:
#> L(TSpread_ts) = 0
#> L(TSpread_ts, 2) = 0
#> 
#> Model 1: restricted model
#> Model 2: GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + L(TSpread_ts) + 
#>     L(TSpread_ts, 2)
#> 
#> Note: Coefficient covariance matrix supplied.
#> 
#>   Res.Df Df      F  Pr(>F)  
#> 1    201                    
#> 2    199  2 4.4344 0.01306 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# set seed
set.seed(1234)

# simulate the time series
Y <- arima.sim(list(order = c(2, 0, 0), ar = c(0.2, 0.2)),  n = 200)

# estimate an AR(2) model using 'arima()', see ?arima
model <- arima(Y, order = c(2, 0, 0))

# compute points forecasts and prediction intervals for the next 25 periods
fc <- forecast(model, h = 25, level = seq(5, 99, 10))

# plot a fan chart
plot(fc, 
     main = "Forecast Fan Chart for AR(2) Model of Simulated Data", 
     showgap = F, 
     fcol = "red",
     flty = 2)


# 14.6 Lag Length Selection Using Information Criteria -------------------------

# compute BIC for AR model objects of class 'dynlm'
BIC <- function(model) {
    
    ssr <- sum(model$residuals^2)
    t <- length(model$residuals)
    npar <- length(model$coef)
    
    return(
        round(c("p" = npar - 1,
                "BIC" = log(ssr/t) + npar * log(t)/t,
                "Adj.R2" = summary(model)$adj.r.squared), 4)
    )
}


# apply the BIC() to an intercept-only model of GDP growth
BIC(dynlm(ts(GDPGR_level) ~ 1))
#>      p    BIC Adj.R2 
#> 0.0000 2.4394 0.0000

# loop BIC over models of different orders
order <- 1:6

BICs <- sapply(order, function(x) 
    "AR" = BIC(dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level), 1:x))))

BICs
#>          [,1]   [,2]   [,3]   [,4]   [,5]   [,6]
#> p      1.0000 2.0000 3.0000 4.0000 5.0000 6.0000
#> BIC    2.3486 2.3475 2.3774 2.4034 2.4188 2.4429
#> Adj.R2 0.1099 0.1339 0.1303 0.1303 0.1385 0.1325


# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])]
#>      p    BIC Adj.R2 
#> 2.0000 2.3475 0.1339


# loop 'BIC()' over multiple ADL models 
order <- 1:12

BICs <- sapply(order, function(x) 
    BIC(dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts, 1:x) + L(TSpread_ts, 1:x), 
              start = c(1962, 1), end = c(2012, 4))))

BICs
#>          [,1]   [,2]   [,3]   [,4]    [,5]    [,6]    [,7]    [,8]    [,9]
#> p      2.0000 4.0000 6.0000 8.0000 10.0000 12.0000 14.0000 16.0000 18.0000
#> BIC    2.3411 2.3408 2.3813 2.4181  2.4568  2.5048  2.5539  2.6029  2.6182
#> Adj.R2 0.1332 0.1692 0.1704 0.1747  0.1773  0.1721  0.1659  0.1586  0.1852
#>          [,10]   [,11]   [,12]
#> p      20.0000 22.0000 24.0000
#> BIC     2.6646  2.7205  2.7664
#> Adj.R2  0.1864  0.1795  0.1810


# select the ADL model with the smallest BIC
BICs[, which.min(BICs[2, ])]
#>      p    BIC Adj.R2 
#> 4.0000 2.3408 0.1692


# 14.7 Nonstationarity I: Trends -----------------------------------------------

# simulate and plot random walks starting at 0
set.seed(1)

RWs <- ts(replicate(n = 4, 
                    arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))

matplot(RWs, 
        type = "l", 
        col = c("steelblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "Four Random Walks",
        xlab = "Time",
        ylab = "Value")


# simulate and plot random walks with drift
set.seed(1)

RWsd <- ts(replicate(n = 4, 
                     arima.sim(model = list(order = c(0, 1, 0)), 
                               n = 100,
                               mean = -0.2)))

matplot(RWsd, 
        type = "l", 
        col = c("steelblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "Four Random Walks with Drift",
        xlab = "Time",
        ylab = "Value")


# plot spurious relationship
matplot(RWs[, c(2, 3)], 
        lty = 1,
        lwd = 2,
        type = "l",
        col = c("darkgreen", "darkred"),
        xlab = "Time",
        ylab = "",
        main = "A Spurious Relationship")    


# estimate spurious AR model
summary(dynlm(RWs[, 2] ~ L(RWs[, 3])))$coefficients
#>              Estimate Std. Error   t value     Pr(>|t|)
#> (Intercept) -3.459488  0.3635104 -9.516889 1.354156e-15
#> L(RWs[, 3])  1.047195  0.1450874  7.217687 1.135828e-10


# plot U.S. unemployment rate & Japanese industrial production
plot(merge(as.zoo(USUnemp), as.zoo(JPIndProd)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Spurious Regression: Macroeconomic Time series")
# add a legend
legend("topleft", 
       legend = c("USUnemp", "JPIndProd"),
       col = c("darkred", "steelblue"),
       lwd = c(2, 2))


# estimate regression using data from 1962 to 1985
SR_Unemp1 <- dynlm(ts(USUnemp["1962::1985"]) ~ ts(JPIndProd["1962::1985"]))
coeftest(SR_Unemp1, vcov = sandwich)
#> 
#> t test of coefficients:
#> 
#>                             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                 -2.37452    1.12041 -2.1193    0.0367 *  
#> ts(JPIndProd["1962::1985"])  2.22057    0.29233  7.5961 2.227e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Estimate regression using data from 1986 to 2012
SR_Unemp2 <- dynlm(ts(USUnemp["1986::2012"]) ~ ts(JPIndProd["1986::2012"]))
coeftest(SR_Unemp2, vcov = sandwich)
#> 
#> t test of coefficients:
#> 
#>                             Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)                  41.7763     5.4066  7.7270 6.596e-12 ***
#> ts(JPIndProd["1986::2012"])  -7.7771     1.1714 -6.6391 1.386e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# repetitions
N <- 1000

# observations
n <- 1000

# define constant, trend and rho
drift <- 0.5
trend <- 1:n
rho <- 1

# function which simulates an AR(1) process
AR1 <- function(rho) {
    out <- numeric(n)
    for(i in 2:n) {
        out[i] <- rho * out[i-1] + rnorm(1)
    }
    return(out)
}

# simulate from DGP with constant 
RWD <- ts(replicate(n = N, drift + AR1(rho)))

# compute ADF test statistics and store them in 'ADFD'
ADFD <- numeric(N)

for(i in 1:ncol(RWD)) {
    ADFD[i] <- summary(
        dynlm(diff(RWD[, i], 1) ~ L(RWD[, i], 1)))$coef[2, 3]
}

# simulate from DGP with constant and trend
RWDT <- ts(replicate(n = N, drift + trend + AR1(rho)))

# compute ADF test statistics and store them in 'ADFDT'
ADFDT <- numeric(N)

for(i in 1:ncol(RWDT)) {
    ADFDT[i] <- summary(
        dynlm(diff(RWDT[, i], 1) ~ L(RWDT[, i], 1) + trend(RWDT[, i]))
    )$coef[2, 3]
}


# estimate quantiles for ADF regression with a drift
round(quantile(ADFD, c(0.1, 0.05, 0.01)), 2)
#>   10%    5%    1% 
#> -2.62 -2.83 -3.39

# estimate quantiles for ADF regression with drift and trend
round(quantile(ADFDT, c(0.1, 0.05, 0.01)), 2)
#>   10%    5%    1% 
#> -3.11 -3.43 -3.97


# plot standard normal density
curve(dnorm(x), 
      from = -6, to = 3, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Density",
      xlab = "t-Statistic",
      main = "Distributions of ADF Test Statistics",
      col = "darkred", 
      lwd = 2)

# plot density estimates of both Dickey-Fuller distributions
lines(density(ADFD), lwd = 2, col = "darkgreen")
lines(density(ADFDT), lwd = 2, col = "blue")

# add a legend
legend("topleft", 
       c("N(0,1)", "Drift", "Drift+Trend"),
       col = c("darkred", "darkgreen", "blue"),
       lty = c(2, 1, 1),
       lwd = 2)


# generate log GDP series
LogGDP <- ts(log(GDP["1962::2012"]))

# estimate the model
coeftest(
    dynlm(diff(LogGDP) ~ trend(LogGDP, scale = F) + L(LogGDP) 
          + diff(L(LogGDP)) + diff(L(LogGDP), 2)))
#> 
#> t test of coefficients:
#> 
#>                             Estimate  Std. Error t value Pr(>|t|)   
#> (Intercept)               0.27877045  0.11793233  2.3638 0.019066 * 
#> trend(LogGDP, scale = F)  0.00023818  0.00011090  2.1476 0.032970 * 
#> L(LogGDP)                -0.03332452  0.01441436 -2.3119 0.021822 * 
#> diff(L(LogGDP))           0.08317976  0.11295542  0.7364 0.462371   
#> diff(L(LogGDP), 2)        0.18763384  0.07055574  2.6594 0.008476 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(LogGDP, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
#> 
#> ############################################### 
#> # Augmented Dickey-Fuller Test Unit Root Test # 
#> ############################################### 
#> 
#> Test regression trend 
#> 
#> 
#> Call:
#> lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.025580 -0.004109  0.000321  0.004869  0.032781 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.2790086  0.1180427   2.364 0.019076 *  
#> z.lag.1     -0.0333245  0.0144144  -2.312 0.021822 *  
#> tt           0.0002382  0.0001109   2.148 0.032970 *  
#> z.diff.lag1  0.2708136  0.0697696   3.882 0.000142 ***
#> z.diff.lag2  0.1876338  0.0705557   2.659 0.008476 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.007704 on 196 degrees of freedom
#> Multiple R-squared:  0.1783, Adjusted R-squared:  0.1616 
#> F-statistic: 10.63 on 4 and 196 DF,  p-value: 8.076e-08
#> 
#> 
#> Value of test-statistic is: -2.3119 11.2558 4.267 
#> 
#> Critical values for test statistics: 
#>       1pct  5pct 10pct
#> tau3 -3.99 -3.43 -3.13
#> phi2  6.22  4.75  4.07
#> phi3  8.43  6.49  5.47


# 14.8 Nonstationarity II: Breaks ----------------------------------------------

# set up a range of possible break dates
tau <- seq(1970, 2005, 0.25)

# initialize vector of F-statistics
Fstats <- numeric(length(tau))

# estimation loop over break dates
for(i in 1:length(tau)) {
    
    # set up dummy variable
    D <- time(GDPGrowth_ts) > tau[i]
    
    # estimate ADL(2,2) model with intercations
    test <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + 
                      D*L(TSpread_ts) + D*L(TSpread_ts, 2),
                  start = c(1962, 1), 
                  end = c(2012, 4))
    
    # compute and save the F-statistic
    Fstats[i] <- linearHypothesis(test, 
                                  c("DTRUE=0", "DTRUE:L(TSpread_ts)", 
                                    "DTRUE:L(TSpread_ts, 2)"),
                                  vcov. = sandwich)$F[2]
    
}


# identify QLR statistic
QLR <- max(Fstats)
QLR
#> [1] 6.651156


# identify the time period where the QLR-statistic is observed
as.yearqtr(tau[which.max(Fstats)])
#> [1] "1980 Q4"


# series of F-statistics
Fstatsseries <- ts(Fstats, 
                   start = tau[1], 
                   end = tau[length(tau)], 
                   frequency = 4)

# plot the F-statistics 
plot(Fstatsseries, 
     xlim = c(1960, 2015),
     ylim = c(1, 7.5),
     lwd = 2,
     col = "steelblue",
     ylab = "F-Statistic",
     xlab = "Break Date",
     main = "Testing for a Break in GDP ADL(2,2) Regression at Different Dates",
     cex.main=0.8)

# dashed horizontal lines for critical values and QLR statistic
abline(h = 4.71, lty = 2)
abline(h = 6.02, lty = 2)
segments(0, QLR, 1980.75, QLR, col = "darkred")
text(2010, 6.2, "1% Critical Value",cex=0.8)
text(2010, 4.9, "5% Critical Value",cex=0.8)
text(1980.75, QLR+0.2, "QLR Statistic",cex=0.8)


# end of sample dates
EndOfSample <- seq(2002.75, 2012.5, 0.25)

# initialize matrix forecasts
forecasts <- matrix(nrow = 1, 
                    ncol = length(EndOfSample))

# initialize vector SER
SER  <- numeric(length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {
    
    # estimate ADL(2,2) model
    m <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) 
               + L(TSpread_ts) + L(TSpread_ts, 2), 
               start = c(1981, 1), 
               end = EndOfSample[i])
    
    SER[i] <- summary(m)$sigma
    
    # sample data for one-period ahead forecast
    s <- window(ADLdata, EndOfSample[i] - 0.25, EndOfSample[i])
    
    # compute forecast
    forecasts[i] <- coef(m) %*% c(1, s[1, 1], s[2, 1], s[1, 2], s[2, 2]) 
}


# compute psuedo-out-of-sample forecast errors
POOSFCE <- c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4))) - forecasts


# series of pseudo-out-of-sample forecasts
PSOSSFc <- ts(c(forecasts), 
              start = 2003, 
              end = 2012.75, 
              frequency = 4)

# plot the GDP growth time series
plot(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     main = "Pseudo-Out-Of-Sample Forecasts of GDP Growth")

# add the series of pseudo-out-of-sample forecasts
lines(PSOSSFc, 
      lwd = 2, 
      lty = 2)

# shade area between curves (the pseudo forecast error)
polygon(c(time(PSOSSFc), rev(time(PSOSSFc))), 
        c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)), rev(PSOSSFc)),
        col = alpha("blue", alpha = 0.3),
        border = NA)

# add a legend
legend("bottomleft", 
       lty = c(1, 2, 1),
       lwd = c(2, 2, 10),
       col = c("steelblue", "black", alpha("blue", alpha = 0.3)), 
       legend = c("Actual GDP growth rate",
                  "Forecasted GDP growth rate",
                  "Pseudo forecast Error"))


# SER of ADL(2,2) mode using data from 1981:Q1 - 2002:Q4
SER[1]
#> [1] 2.389773


# compute root mean squared forecast error
sd(POOSFCE)
#> [1] 2.667612


# test if mean forecast error is zero
t.test(POOSFCE)
#> 
#>  One Sample t-test
#> 
#> data:  POOSFCE
#> t = -1.5523, df = 39, p-value = 0.1287
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  -1.5078876  0.1984001
#> sample estimates:
#>  mean of x 
#> -0.6547438


# 14.9 Can You Beat the Market? (Part II) --------------------------------------

# plot logarithm of dividend yield series
plot(StockReturns[, 2], 
     col = "steelblue", 
     lwd = 2, 
     ylab = "Logarithm", 
     main = "Dividend Yield for CRSP Index")


# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(window(StockReturns[, 2], 
                     c(1960,1), 
                     c(2002, 12)), 
              type = "drift", 
              lags = 0))
#> 
#> ############################################### 
#> # Augmented Dickey-Fuller Test Unit Root Test # 
#> ############################################### 
#> 
#> Test regression drift 
#> 
#> 
#> Call:
#> lm(formula = z.diff ~ z.lag.1 + 1)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -14.3540  -2.9118  -0.2953   2.6375  25.5169 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)
#> (Intercept) -2.740964   2.080038  -1.318    0.188
#> z.lag.1     -0.007652   0.005989  -1.278    0.202
#> 
#> Residual standard error: 4.45 on 513 degrees of freedom
#> Multiple R-squared:  0.003172,   Adjusted R-squared:  0.001229 
#> F-statistic: 1.633 on 1 and 513 DF,  p-value: 0.2019
#> 
#> 
#> Value of test-statistic is: -1.2777 0.9339 
#> 
#> Critical values for test statistics: 
#>       1pct  5pct 10pct
#> tau2 -3.43 -2.86 -2.57
#> phi1  6.43  4.59  3.78


# ADL(1,1) (1st difference of log dividend yield)
CRSP_ADL_1 <- dynlm(ExReturn ~ L(ExReturn) + d(L(ln_DivYield)), 
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(2,2) (1st & 2nd differences of log dividend yield)
CRSP_ADL_2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2) 
                    + d(L(ln_DivYield)) + d(L(ln_DivYield, 2)), 
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(1,1) (level of log dividend yield)
CRSP_ADL_3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield),
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))


# gather robust standard errors
rob_se_CRSP_ADL <- list(sqrt(diag(sandwich(CRSP_ADL_1))),
                        sqrt(diag(sandwich(CRSP_ADL_2))),
                        sqrt(diag(sandwich(CRSP_ADL_3))))


stargazer(CRSP_ADL_1, CRSP_ADL_2, CRSP_ADL_3,
          title = "ADL Models of Monthly Excess Stock Returns",
          header = FALSE, 
          # type = "latex",
          column.sep.width = "-5pt",
          no.space = T,
          digits = 3, 
          column.labels = c("ADL(1,1)", "ADL(2,2)", "ADL(1,1)"),
          dep.var.caption  = "Dependent Variable: Excess returns on the CSRP value-weighted index",
          dep.var.labels.include = FALSE,
          covariate.labels = c("$excess return_{t-1}$", 
                               "$excess return_{t-2}$", 
                               "$1^{st} diff log(dividend yield_{t-1})$", 
                               "$1^{st} diff log(dividend yield_{t-2})$", 
                               "$log(dividend yield_{t-1})$", 
                               "Constant"),
          se = rob_se_CRSP_ADL,
          type = "html", out = "output/table_CH14_2.html")


# end of sample dates
EndOfSample <- as.numeric(window(time(StockReturns), c(1992, 12), c(2002, 11)))

# initialize matrix  forecasts
forecasts <- matrix(nrow = 2, 
                    ncol = length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {
    
    # estimate model (3)
    mod3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield), data = StockReturns, 
                  start = c(1960, 1), 
                  end = EndOfSample[i])
    
    # estimate intercept only model
    modconst <- dynlm(ExReturn ~ 1, data = StockReturns, 
                      start = c(1960, 1), 
                      end = EndOfSample[i])
    
    # sample data for one-period ahead forecast
    t <- window(StockReturns, EndOfSample[i], EndOfSample[i])
    
    # compute forecast
    forecasts[, i] <- c(coef(mod3) %*% c(1, t[1], t[2]), coef(modconst))
    
}


# gather data
d <- cbind("Excess Returns" = c(window(StockReturns[,1], c(1993, 1), c(2002, 12))),
           "Model (3)" = forecasts[1,], 
           "Intercept Only" = forecasts[2,], 
           "Always Zero" =  0)

# Compute RMSFEs
c("ADL model (3)" = sd(d[, 1] - d[, 2]),
  "Intercept-only model" = sd(d[, 1] - d[, 3]),
  "Always zero" = sd(d[,1] - d[, 4]))
#>        ADL model (3) Intercept-only model          Always zero 
#>             4.043754             4.000218             3.995425
