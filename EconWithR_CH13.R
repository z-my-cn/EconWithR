# Econometrics with R
# Chapter 13 Experiments and Quasi-Experiments ---------------------------------
library(AER)
library(MASS)
library(mvtnorm)
library(rddtools)
library(scales)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(foreign)


# 13.1 Potential Outcomes, Causal Effects and Idealized Experiments ------------


# 13.2 Threats to Validity of Experiments


# 13.3 Experimental Estimates of the Effect of Class Size Reductions -----------

# load the package AER and the STAR dataset
# library(AER)
data(STAR)
STAR <- as_tibble(STAR)


# get an overview
head(STAR, 2)
#> # A tibble: 2 × 47
#>   gender ethnicity birth   stark star1 star2 star3 readk read1 read2 read3 mathk
#>   <fct>  <fct>     <yearq> <fct> <fct> <fct> <fct> <int> <int> <int> <int> <int>
#> 1 female afam      1979 Q3 NA    NA    NA    regu…    NA    NA    NA   580    NA
#> 2 female cauc      1980 Q1 small small small small   447   507   568   587   473
#> # ℹ 35 more variables: math1 <int>, math2 <int>, math3 <int>, lunchk <fct>,
#> #   lunch1 <fct>, lunch2 <fct>, lunch3 <fct>, schoolk <fct>, school1 <fct>,
#> #   school2 <fct>, school3 <fct>, degreek <fct>, degree1 <fct>, degree2 <fct>,
#> #   degree3 <fct>, ladderk <fct>, ladder1 <fct>, ladder2 <fct>, ladder3 <fct>,
#> #   experiencek <int>, experience1 <int>, experience2 <int>, experience3 <int>,
#> #   tethnicityk <fct>, tethnicity1 <fct>, tethnicity2 <fct>, tethnicity3 <fct>,
#> #   systemk <fct>, system1 <fct>, system2 <fct>, system3 <fct>, …
dim(STAR)
#> [1] 11598    47


# get variable names
names(STAR)
#>  [1] "gender"      "ethnicity"   "birth"       "stark"       "star1"      
#>  [6] "star2"       "star3"       "readk"       "read1"       "read2"      
#> [11] "read3"       "mathk"       "math1"       "math2"       "math3"      
#> [16] "lunchk"      "lunch1"      "lunch2"      "lunch3"      "schoolk"    
#> [21] "school1"     "school2"     "school3"     "degreek"     "degree1"    
#> [26] "degree2"     "degree3"     "ladderk"     "ladder1"     "ladder2"    
#> [31] "ladder3"     "experiencek" "experience1" "experience2" "experience3"
#> [36] "tethnicityk" "tethnicity1" "tethnicity2" "tethnicity3" "systemk"    
#> [41] "system1"     "system2"     "system3"     "schoolidk"   "schoolid1"  
#> [46] "schoolid2"   "schoolid3"


# drop NA recordings for the first observation and print to the console
# STAR[1, !is.na(STAR[1, ])]
STAR %>% slice(1) %>% select_if(~ !is.na(.))
#> # A tibble: 1 × 14
#>   gender ethnicity birth     star3   read3 math3 lunch3 school3  degree3 ladder3
#>   <fct>  <fct>     <yearqtr> <fct>   <int> <int> <fct>  <fct>    <fct>   <fct>  
#> 1 female afam      1979 Q3   regular   580   564 free   suburban bachel… level1 
#> # ℹ 4 more variables: experience3 <int>, tethnicity3 <fct>, system3 <fct>,
#> #   schoolid3 <fct>


# compute differences Estimates for each grades
fmk <- lm(I(readk + mathk) ~ stark, data = STAR)
fm1 <- lm(I(read1 + math1) ~ star1, data = STAR)
fm2 <- lm(I(read2 + math2) ~ star2, data = STAR)
fm3 <- lm(I(read3 + math3) ~ star3, data = STAR)


# obtain coefficient matrix using robust standard errors
coeftest(fmk, vcov = vcovHC, type= "HC1")
#> 
#> t test of coefficients:
#> 
#>                    Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)       918.04289    1.63339 562.0473 < 2.2e-16 ***
#> starksmall         13.89899    2.45409   5.6636 1.554e-08 ***
#> starkregular+aide   0.31394    2.27098   0.1382    0.8901    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(fm1, vcov = vcovHC, type= "HC1")
#> 
#> t test of coefficients:
#> 
#>                    Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)       1039.3926     1.7846 582.4321 < 2.2e-16 ***
#> star1small          29.7808     2.8311  10.5190 < 2.2e-16 ***
#> star1regular+aide   11.9587     2.6520   4.5093  6.62e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(fm2, vcov = vcovHC, type= "HC1")
#> 
#> t test of coefficients:
#> 
#>                    Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)       1157.8066     1.8151 637.8820 < 2.2e-16 ***
#> star2small          19.3944     2.7117   7.1522  9.55e-13 ***
#> star2regular+aide    3.4791     2.5447   1.3672    0.1716    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(fm3, vcov = vcovHC, type= "HC1")
#> 
#> t test of coefficients:
#> 
#>                     Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)       1228.50636    1.68001 731.2483 < 2.2e-16 ***
#> star3small          15.58660    2.39604   6.5051 8.393e-11 ***
#> star3regular+aide   -0.29094    2.27271  -0.1280    0.8981    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# compute robust standard errors for each model and gather them in a list
rob_se_1 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
                 sqrt(diag(vcovHC(fm1, type = "HC1"))),
                 sqrt(diag(vcovHC(fm2, type = "HC1"))),
                 sqrt(diag(vcovHC(fm3, type = "HC1"))))


# library(stargazer)

stargazer(fmk,fm1,fm2,fm3,
          title = "Project STAR: Differences Estimates",
          header = FALSE, 
          # type = "latex",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("K", "1", "2", "3"),
          dep.var.caption  = "Dependent Variable: Grade",
          dep.var.labels.include = FALSE,
          se = rob_se_1,
          type = "html", out = "output/table_CH13_1.html")


# load packages 'dplyr' and 'tidyr' for data wrangling functionalities
# library(dplyr)
# library(tidyr)

# generate subset with kindergarten data
STARK <- STAR %>% 
    transmute(gender,
              ethnicity,
              stark,
              readk,
              mathk,
              lunchk,
              experiencek,
              schoolidk) %>% 
    mutate(black = ifelse(ethnicity == "afam", 1, 0),
           race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0),
           boy = ifelse(gender == "male", 1, 0))


# estimate the models 
gradeK1 <- lm(I(mathk + readk) ~ stark + experiencek, 
              data = STARK)

gradeK2 <- lm(I(mathk + readk) ~ stark + experiencek + schoolidk, 
              data = STARK)

gradeK3 <- lm(I(mathk + readk) ~ stark + experiencek + boy + lunchk 
              + black + race + schoolidk, 
              data = STARK)


# obtain robust inference on the significance of coefficients
coeftest(gradeK1, vcov. = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>                    Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept)       904.72124    2.22235 407.1020 < 2.2e-16 ***
#> starksmall         14.00613    2.44704   5.7237 1.095e-08 ***
#> starkregular+aide  -0.60058    2.25430  -0.2664    0.7899    
#> experiencek         1.46903    0.16929   8.6778 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(gradeK2, vcov. = vcovHC, type = "HC1")[1:4, ]
#>                      Estimate Std. Error     t value     Pr(>|t|)
#> (Intercept)       925.6748750  7.6527218 120.9602155 0.000000e+00
#> starksmall         15.9330822  2.2411750   7.1092540 1.310324e-12
#> starkregular+aide   1.2151960  2.0353415   0.5970477 5.504993e-01
#> experiencek         0.7431059  0.1697619   4.3773429 1.222880e-05
coeftest(gradeK3, vcov. = vcovHC, type = "HC1")[1:7, ]
#>                      Estimate Std. Error     t value     Pr(>|t|)
#> (Intercept)       937.6831330 14.3726687  65.2407117 0.000000e+00
#> starksmall         15.8900507  2.1551817   7.3729516 1.908960e-13
#> starkregular+aide   1.7869378  1.9614592   0.9110247 3.623211e-01
#> experiencek         0.6627251  0.1659298   3.9940097 6.578846e-05
#> boy               -12.0905123  1.6726331  -7.2284306 5.533119e-13
#> lunchkfree        -34.7033021  1.9870366 -17.4648529 1.437931e-66
#> black             -25.4305130  3.4986918  -7.2685776 4.125252e-13


# compute robust standard errors for each model and gather them in a list
rob_se_2 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK1, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK2, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK3, type = "HC1"))))


stargazer(fmk, gradeK1, gradeK2, gradeK3,
          title = "Project STAR - Differences Estimates with 
          Additional Regressors for Kindergarten",
          header = FALSE, 
          # type = "latex",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("(1)", "(2)", "(3)", "(4)"),
          dep.var.caption  = "Dependent Variable: Test Score in Kindergarten",
          dep.var.labels.include = FALSE,
          se = rob_se_2,
          omit = "schoolidk",
          add.lines = list(c("School indicators? ", "No", "No", "Yes", "Yes")),
          type = "html", out = "output/table_CH13_2.html")


# compute the sample standard deviations of test scores
SSD <- c("K" = sd(na.omit(STAR$readk + STAR$mathk)),
         "1" = sd(na.omit(STAR$read1 + STAR$math1)),
         "2" = sd(na.omit(STAR$read2 + STAR$math2)),
         "3" = sd(na.omit(STAR$read3 + STAR$math3)))

# translate the effects of small classes to standard deviations
Small <- c("K" = as.numeric(coef(fmk)[2]/SSD[1]),
           "1" = as.numeric(coef(fm1)[2]/SSD[2]),
           "2" = as.numeric(coef(fm2)[2]/SSD[3]),
           "3" = as.numeric(coef(fm3)[2]/SSD[4]))

# adjust the standard errors
SmallSE <- c("K" = as.numeric(rob_se_1[[1]][2]/SSD[1]),
             "1" = as.numeric(rob_se_1[[2]][2]/SSD[2]),
             "2" = as.numeric(rob_se_1[[3]][2]/SSD[3]),
             "3" = as.numeric(rob_se_1[[4]][2]/SSD[4]))

# translate the effects of regular classes with aide to standard deviations
RegAide<- c("K" = as.numeric(coef(fmk)[3]/SSD[1]),
            "1" = as.numeric(coef(fm1)[3]/SSD[2]),
            "2" = as.numeric(coef(fm2)[3]/SSD[3]),
            "3" = as.numeric(coef(fm3)[3]/SSD[4]))

# adjust the standard errors
RegAideSE <- c("K" = as.numeric(rob_se_1[[1]][3]/SSD[1]),
               "1" = as.numeric(rob_se_1[[2]][3]/SSD[2]),
               "2" = as.numeric(rob_se_1[[3]][3]/SSD[3]),
               "3" = as.numeric(rob_se_1[[4]][3]/SSD[4]))

# gather the results in a data.frame and round
df <- t(round(data.frame(
    Small, SmallSE, RegAide, RegAideSE, SSD),
    digits =  2))


# generate a simple table using stargazer
stargazer(df,
          title = "Estimated Class Size Effects 
          (in Units of Standard Deviations)",
          type = "html", 
          summary = FALSE,
          header = FALSE, 
          out = "output/table_CH13_3.html")


# 13.4 Quasi Experiments -------------------------------------------------------

# initialize plot and add control group
# plot(c(0, 1), c(6, 8), 
#      type = "p",
#      ylim = c(5, 12),
#      xlim = c(-0.3, 1.3),
#      main = "The Differences-in-Differences Estimator",
#      xlab = "Period",
#      ylab = "Y",
#      col = "steelblue",
#      pch = 20,
#      xaxt = "n",
#      yaxt = "n")
# 
# axis(1, at = c(0, 1), labels = c("before", "after"))
# axis(2, at = c(0, 13))

# add treatment group
# points(c(0, 1, 1), c(7, 9, 11), 
#        col = "darkred",
#        pch = 20)

# add line segments
# lines(c(0, 1), c(7, 11), col = "darkred")
# lines(c(0, 1), c(6, 8), col = "steelblue")
# lines(c(0, 1), c(7, 9), col = "darkred", lty = 2)
# lines(c(1, 1), c(9, 11), col = "black", lty = 2, lwd = 2)

# add annotations
# text(1, 10, expression(hat(beta)[1]^{DID}), cex = 0.8, pos = 4)
# text(0, 5.5, "s. mean control", cex = 0.8 , pos = 4)
# text(0, 6.8, "s. mean treatment", cex = 0.8 , pos = 4)
# text(1, 7.9, "s. mean control", cex = 0.8 , pos = 4)
# text(1, 11.1, "s. mean treatment", cex = 0.8 , pos = 4)
ggplot() +
  geom_point(aes(x = 0, y = 6), col = "steelblue", size = 3) +
  geom_point(aes(x = 1, y = 8), col = "steelblue", size = 3) +
  geom_point(aes(x = 0, y = 7), col = "darkred", size = 3) +
  geom_point(aes(x = 1, y = 9), col = "darkred", size = 3) +
  geom_point(aes(x = 1, y = 11), col = "darkred", size = 3) +
  geom_line(aes(x = c(0, 1), y = c(6, 8)), col = "steelblue") +
  geom_line(aes(x = c(0, 1), y = c(7, 11)), col = "darkred") +
  geom_line(aes(x = c(0, 1), y = c(7, 9)), col = "darkred", 
            linetype = "dashed") +
  geom_line(aes(x = c(1, 1), y = c(9, 11)), col = "black", 
            linetype = "dashed") +
  annotate("text", x = 1, y = 10, parse = TRUE, label = "hat(beta)[1]^{DID}",
           hjust = 0) +
  annotate("text", x = 0, y = 5.8, label = "s. mean control", hjust = 0) +
  annotate("text", x = 0, y = 6.8, label = "s. mean treatment", hjust = 0) +
  annotate("text", x = 1, y = 7.8, label = "s. mean control", hjust = 0) +
  annotate("text", x = 1, y = 11.3, label = "s. mean treatment", hjust = 0) +
  labs(title = "The Differences-in-Differences Estimator",
       x = "Period", y = "Y") +
  scale_x_continuous(limits = c(-0.1, 1.3), breaks = c(0, 1), 
                     labels = c("before", "after")) +
  scale_y_continuous(limits = c(5, 12)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"))


# set sample size
n <- 200

# define treatment effect
TEffect <- 4

# generate treatment dummy
TDummy <- c(rep(0, n/2), rep(1, n/2))

# simulate pre- and post-treatment values of the dependent variable
y_pre <- 7 + rnorm(n)
y_pre[1:n/2] <- y_pre[1:n/2] - 1
y_post <- 7 + 2 + TEffect * TDummy + rnorm(n)
y_post[1:n/2] <- y_post[1:n/2] - 1 


# library(scales)

pre <- rep(0, length(y_pre[TDummy==0]))
post <- rep(1, length(y_pre[TDummy==0]))

# plot control group in t=1
# plot(jitter(pre, 0.6), 
#      y_pre[TDummy == 0], 
#      ylim = c(0, 16), 
#      col = alpha("steelblue", 0.3),
#      pch = 20, 
#      xlim = c(-0.5, 1.5),
#      ylab = "Y",
#      xlab = "Period",
#      xaxt = "n",
#      main = "Artificial Data for DID Estimation")
# 
# axis(1, at = c(0, 1), labels = c("before", "after"))

# add treatment group in t=1
# points(jitter(pre, 0.6), 
#        y_pre[TDummy == 1], 
#        col = alpha("darkred", 0.3), 
#        pch = 20)

# add control group in t=2
# points(jitter(post, 0.6),
#        y_post[TDummy == 0], 
#        col = alpha("steelblue", 0.5),
#        pch = 20)

# add treatment group in t=2
# points(jitter(post, 0.6), 
#        y_post[TDummy == 1], 
#        col = alpha("darkred", 0.5),
#        pch = 20)
ggplot() +
  geom_jitter(aes(x = pre, y = y_pre[TDummy == 0], col = "control"), 
             size = 3, alpha = 0.3, width = 0.02) +
  geom_jitter(aes(x = pre, y = y_pre[TDummy == 1], col = "treatment"),
             size = 3, alpha = 0.3, width = 0.02) +
  geom_jitter(aes(x = post, y = y_post[TDummy == 0], col = "control"),
             size = 3, alpha = 0.5, width = 0.02) +
  geom_jitter(aes(x = post, y = y_post[TDummy == 1], col = "treatment"),
             size = 3, alpha = 0.5, width = 0.02) +
  labs(title = "Artificial Data for DID Estimation",
       x = "Period", y = "Y") +
  scale_color_manual(name = "Group",
                     values = c("control" = "steelblue", 
                                "treatment" = "darkred")) +
  scale_x_continuous(limits = c(-0.5, 1.5), breaks = c(0, 1), 
                     labels = c("before", "after")) +
  scale_y_continuous(limits = c(0, 16)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"))


# compute the DID estimator for the treatment effect 'by hand'
mean(y_post[TDummy == 1]) - mean(y_pre[TDummy == 1]) - 
    (mean(y_post[TDummy == 0]) - mean(y_pre[TDummy == 0]))
#> [1] 3.960268


# compute the DID estimator using a linear model
lm(I(y_post - y_pre) ~ TDummy)
#> 
#> Call:
#> lm(formula = I(y_post - y_pre) ~ TDummy)
#> 
#> Coefficients:
#> (Intercept)       TDummy  
#>       2.104        3.960


# prepare data for DID regression using the interaction term 
d <- data.frame("Y" = c(y_pre,y_post),
                "Treatment" = TDummy, 
                "Period" = c(rep("1", n), rep("2", n)))

# estimate the model
lm(Y ~ Treatment * Period, data = d)
#> 
#> Call:
#> lm(formula = Y ~ Treatment * Period, data = d)
#> 
#> Coefficients:
#>       (Intercept)          Treatment            Period2  Treatment:Period2  
#>             5.858              1.197              2.104              3.960


# generate some sample data
W <- runif(1000, -1, 1)
y <- 3 + 2 * W + 10 * (W>=0) + rnorm(1000)


# load the package 'rddtools'
# library(rddtools)

# construct rdd_data 
data <- rdd_data(y, W, cutpoint = 0)

# plot the sample data
plot(data,
     col = "steelblue",
     cex = 0.35, 
     xlab = "W", 
     ylab = "Y")


# estimate the sharp RDD model
rdd_mod <- rdd_reg_lm(rdd_object = data, 
                      slope = "same")
summary(rdd_mod)
#> 
#> Call:
#> lm(formula = y ~ ., data = dat_step1, weights = weights)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.2361 -0.6779 -0.0039  0.7113  3.0096 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  2.93889    0.07082   41.50   <2e-16 ***
#> D           10.12692    0.12631   80.18   <2e-16 ***
#> x            1.88249    0.11074   17.00   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.019 on 997 degrees of freedom
#> Multiple R-squared:  0.972,  Adjusted R-squared:  0.972 
#> F-statistic: 1.732e+04 on 2 and 997 DF,  p-value: < 2.2e-16


# plot the RDD model along with binned observations
plot(rdd_mod,
     cex = 0.35, 
     col = "steelblue", 
     xlab = "W", 
     ylab = "Y")


# library(MASS)

# generate sample data
mu <- c(0, 0)
sigma <- matrix(c(1, 0.7, 0.7, 1), ncol = 2)

set.seed(1234)
d <- as.data.frame(mvrnorm(2000, mu, sigma))
colnames(d) <- c("W", "Y")

# introduce fuzziness
d$treatProb <- ifelse(d$W < 0, 0, 0.8)

fuzz <- sapply(X = d$treatProb, FUN = function(x) rbinom(1, 1, prob = x))

# treatment effect
d$Y <- d$Y + fuzz * 2


# generate a colored plot of treatment and control group
# plot(d$W, d$Y,
#      col = c("steelblue", "darkred")[factor(fuzz)], 
#      pch= 20, 
#      cex = 0.5,
#      xlim = c(-3, 3),
#      ylim = c(-3.5, 5),
#      xlab = "W",
#      ylab = "Y")

# add a dashed vertical line at cutoff
# abline(v = 0, lty = 2)
#add legend
# legend("topleft",pch=20,col=c("steelblue","darkred"),
#        legend=c("Do not receive treatment","Receive treatment"))
ggplot(d, aes(x = W, y = Y, col = factor(fuzz))) +
  geom_point(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(name = NULL,
                     values = c("0" = "steelblue", 
                                "1" = "darkred"),
                     labels = c("0" = "Do not receive treatment",
                                "1" = "Receive treatment")) +
  labs(x = "W", y = "Y") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# estimate the Fuzzy RDD
data <- rdd_data(d$Y, d$W, 
                 cutpoint = 0, 
                 z = d$treatProb)

frdd_mod <- rdd_reg_lm(rdd_object = data, 
                       slope = "same")
frdd_mod
#> ### RDD regression: parametric ###
#>  Polynomial order:  1 
#>  Slopes:  same 
#>  Number of obs: 2000 (left: 999, right: 1001)
#> 
#>  Coefficient:
#>   Estimate Std. Error t value  Pr(>|t|)    
#> D 1.981297   0.084696  23.393 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# plot estimated FRDD function
plot(frdd_mod, 
     cex = 0.5, 
     lwd = 0.4,
     xlim = c(-4, 4),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")


# estimate SRDD
data <- rdd_data(d$Y, 
                 d$W, 
                 cutpoint = 0)

srdd_mod <- rdd_reg_lm(rdd_object = data, 
                       slope = "same")
srdd_mod
#> ### RDD regression: parametric ###
#>  Polynomial order:  1 
#>  Slopes:  same 
#>  Number of obs: 2000 (left: 999, right: 1001)
#> 
#>  Coefficient:
#>   Estimate Std. Error t value  Pr(>|t|)    
#> D 1.585038   0.067756  23.393 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# 13.5 Exercises ---------------------------------------------------------------

# 1. The Data from Card & Krueger (1994)
# attach the packages
# library(dplyr)
# library(foreign)

# read in the dataset and assign it to `dat`
dat <- read.dta('data/fastfood.dta')
dat <- as_tibble(dat)

# add FTE and FTE2 to `dat`
dat <- dat %>% 
    mutate(FTE = nmgrs + empft + (0.5 * emppt),
           FTE2 = nmgrs2 + empft2 + (0.5 * emppt2))


# 2. State Specific Estimates of Full-Time Employment - I
# generate the subsets
# dat_NJ <- subset(dat, state == 1)
# dat_PA <- subset(dat, state == 0)
dat_NJ <- dat %>% 
    filter(state == 1)
dat_PA <- dat %>%
    filter(state == 0)

# compute the group means
dat %>% 
    group_by(state) %>% 
    summarise(mean(FTE, na.rm = T),
              mean(FTE2, na.rm = T))
#> # A tibble: 2 × 3
#>   state `mean(FTE, na.rm = T)` `mean(FTE2, na.rm = T)`
#>   <int>                  <dbl>                   <dbl>
#> 1     0                   23.3                    21.2
#> 2     1                   20.4                    21.0


# 3. State Specific Estimates of Full-Time Employment - II
# test if the difference in mean employment in New Jersey is zero before and after the wage increase
t.test(dat_NJ$FTE, dat_NJ$FTE2)
#> 	Welch Two Sample t-test
#> 
#> data:  dat_NJ$FTE and dat_NJ$FTE2
#> t = -0.80843, df = 637.55, p-value = 0.4191
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -2.0163300  0.8402872
#> sample estimates:
#> mean of x mean of y 
#>  20.43941  21.02743 


# 4. Preparing the Data for Regression
# set up the data for regression analysis
# reg_dat <- data.frame(
#     rbind(
#         data.frame(id = dat$sheet, 
#                    chain = dat$chain,
#                    state = dat$state,
#                    empl = dat$FTE,
#                    D = 0),
#         data.frame(id = dat$sheet,
#                    chain = dat$chain,
#                    state = dat$state,
#                    empl = dat$FTE2,
#                    D = 1)))
reg_dat <- bind_rows(
    dat %>% 
        select(id = sheet, chain, state, empl = FTE) %>%
        mutate(D = 0),
    dat %>%
        select(id = sheet, chain, state, empl = FTE2) %>%
        mutate(D = 1))


# 5. A Difference Estimate using Data from Card & Krueger (1994) - II
# estimate the regression model
emp_mod <- lm(empl ~ D, data = reg_dat, subset = state == 1)

# obtain a robust summary
# library(AER)
coeftest(emp_mod, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 20.43941    0.50826 40.2142   <2e-16 ***
#> D            0.58802    0.72736  0.8084   0.4191    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 6. A Difference Estimate using Data from Card & Krueger (1994) - II
# check that estimates and t-statistics coincide
t_test <- t.test(dat_NJ$FTE, dat_NJ$FTE2, var.equal = F)
emp_mod <- lm(empl ~ D, data = reg_dat, subset = state == 1)

# estimates
emp_mod$coef[2]
#>         D
#> 0.5880214 
diff(t_test$estimate)
#> mean of y 
#> 0.5880214 

# t-statistics
t_test$statistic
#>          t 
#> -0.8084337 
coeftest(emp_mod, vcov. = vcovHC, type = "HC1")[2,3]
#> [1] 0.8084338


# 7. A Difference-in-Differences Estimate - II
# estimate the DID model 
emp_did_mod <- lm(empl ~ D*state, data = reg_dat)

# obtain a robust summary of the model
coeftest(emp_did_mod, vcov. = vcovHC, type = "HC1")
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  23.3312     1.3457 17.3370  < 2e-16 ***
#> D            -2.1656     1.6412 -1.3195  0.18738    
#> state        -2.8918     1.4387 -2.0100  0.04477 *  
#> D:state       2.7536     1.7955  1.5337  0.12551    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
