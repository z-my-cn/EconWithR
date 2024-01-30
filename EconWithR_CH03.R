# Econometrics with R
# Chapter 03 A Review of Statistics using R ------------------------------------

library(MASS)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(readxl)

# 3.1 Estimation of the Population Mean ----------------------------------------

# plot the chi_12^2 distribution
# curve(dchisq(x, df=12), 
#       from = 0, 
#       to = 40, 
#       ylab = "Density", 
#       xlab = "Hourly earnings in Euro")
ggplot(tibble(x = seq(0, 40, 0.1)), aes(x)) +
  stat_function(fun = dchisq, args = list(df = 12)) +
  labs(x = "Hourly earnings in Euro", y = "Density")


# set seed for reproducibility
set.seed(1)

# sample from the chi_12^2 distribution, use only the first observation
rsamp <- rchisq(n = 100, df = 12)
rsamp[1]
#> [1] 8.257893


# 3.2 Properties of the Sample Mean --------------------------------------------

# generate a fictious population
pop <- rnorm(10000, 10, 1)

# sample from the population and estimate the mean
est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000)

est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000)

fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000)


# check if object type is vector
is.vector(est1)
#> [1] TRUE
is.vector(est2)
#> [1] TRUE

# check length
length(est1)
#> [1] 25000
length(est2)
#> [1] 25000


# plot density estimate Y_1
# plot(density(fo), 
#      col = "green", 
#      lwd = 2,
#      ylim = c(0, 2),
#      xlab = "Estimates",
#      main = "Sampling Distributions of Unbiased Estimators")

# add density estimate for the distribution of the sample mean with n=5 to the plot
# lines(density(est1), 
#       col = "steelblue", 
#       lwd = 2, 
#       bty = "l")

# add density estimate for the distribution of the sample mean with n=25 to the plot
# lines(density(est2), 
#       col = "red2", 
#       lwd = 2)

# add a vertical line at the true parameter
# abline(v = 10, lty = 2)

# add N(10,1) density to the plot
# curve(dnorm(x, mean = 10), 
#       lwd = 2,
#       lty = 2,
#       add = T)

# add a legend
# legend("topleft",
#        legend = c("N(10,1)",
#                   expression(Y[n == 1]),
#                   expression(bar(Y)[n == 5]),
#                   expression(bar(Y)[n == 25])
#        ), 
#        lty = c(2, 1, 1, 1), 
#        col = c("black","green", "steelblue", "red2"),
#        lwd = 2)
ggplot() +
  stat_function(fun = dnorm, args = list(mean = 10), aes(color = "N(10,1)")) +
  stat_density(aes(x = fo, color = "Y[n == 1]"), geom = "line") +
  stat_density(aes(x = est1, color = "bar(Y)[n == 5]"), geom = "line") +
  stat_density(aes(x = est2, color = "bar(Y)[n == 25]"), geom = "line") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  labs(x = "Estimates", y = "Density") +
  scale_color_manual(name = "Distribution", 
                     values = c("N(10,1)" = "black", 
                                "Y[n == 1]" = "green", 
                                "bar(Y)[n == 5]" = "steelblue", 
                                "bar(Y)[n == 25]" = "red2"),
                     labels = c("N(10,1)",
                                expression(Y[n == 1]),
                                expression(bar(Y)[n == 5]),
                                expression(bar(Y)[n == 25]))) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# define the function and vectorize it
sqm <- function(m) {
    sum((y-m)^2)
}
sqm <- Vectorize(sqm)

# draw random sample and compute the mean
y <- rnorm(100, 10, 1)
mean(y)
#> [1] 10.1364

# plot the objective function
# curve(sqm(x), 
#       from = -50, 
#       to = 70,
#       xlab = "m",
#       ylab = "sqm(m)")

# add vertical line at mean(y)
# abline(v = mean(y), 
#        lty = 2, 
#        col = "darkred")

# add annotation at mean(y)
# text(x = mean(y), 
#      y = 0, 
#      labels = paste(round(mean(y), 2)))
ggplot() +
  stat_function(fun = sqm, xlim = c(-50, 70)) +
  geom_vline(xintercept = mean(y), linetype = "dashed", color = "darkred") +
  labs(x = "m", y = "sqm(m)") +
  annotate("text", x = mean(y), y = 0, label = paste(round(mean(y), 2)))


# compute the population mean of pop
mean(pop)
#> [1] 9.992604


# simulate outcomes for the sample mean when the i.i.d. assumption fails
est3 <-  replicate(n = 25000, 
                   expr = mean(sample(x = sort(pop), 
                                      size = 25, 
                                      prob = c(rep(4, 2500), rep(1, 7500)))))

# compute the sample mean of the outcomes
mean(est3)
#> [1] 9.443067


# sampling distribution of sample mean, i.i.d. holds, n=25
# plot(density(est2), 
#      col = "steelblue",
#      lwd = 2,
#      xlim = c(8, 11),
#      xlab = "Estimates",
#      main = "When the i.i.d. Assumption Fails")

# sampling distribution of sample mean, i.i.d. fails, n=25
# lines(density(est3),
#       col = "red2",
#       lwd = 2)

# add a legend
# legend("topleft",
#        legend = c(expression(bar(Y)[n == 25]~", i.i.d. fails"),
#                   expression(bar(Y)[n == 25]~", i.i.d. holds")
#        ), 
#        lty = c(1, 1), 
#        col = c("red2", "steelblue"),
#        lwd = 2)
ggplot() +
  stat_density(aes(x = est2, color = "bar(Y)[n == 25], i.i.d. holds"), geom = "line") +
  stat_density(aes(x = est3, color = "bar(Y)[n == 25], i.i.d. fails"), geom = "line") +
  labs(x = "Estimates", y = "Density",
       title = "When the i.i.d. Assumption Fails") +
  scale_color_manual(name = "Distribution", 
                     values = c("bar(Y)[n == 25], i.i.d. holds" = "steelblue",
                                "bar(Y)[n == 25], i.i.d. fails" = "red2"),
                     labels = c(expression(bar(Y)[n == 25]~", i.i.d. fails"),
                                expression(bar(Y)[n == 25]~", i.i.d. holds"))) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# 3.3 Hypothesis Tests Concerning the Population Mean --------------------------

# plot the standard normal density on the interval [-4,4]
# curve(dnorm(x),
#       xlim = c(-4, 4),
#       main = "Calculating a p-Value",
#       yaxs = "i",
#       xlab = "z",
#       ylab = "",
#       lwd = 2,
#       axes = "F")

# add x-axis
# axis(1, 
#      at = c(-1.5, 0, 1.5), 
#      padj = 0.75,
#      labels = c(expression(-frac(bar(Y)^"act"~-~bar(mu)["Y,0"], sigma[bar(Y)])),
#                 0,
#                 expression(frac(bar(Y)^"act"~-~bar(mu)["Y,0"], sigma[bar(Y)]))))

# shade p-value/2 region in left tail
# polygon(x = c(-6, seq(-6, -1.5, 0.01), -1.5),
#         y = c(0, dnorm(seq(-6, -1.5, 0.01)),0), 
#         col = "steelblue")

# shade p-value/2 region in right tail
# polygon(x = c(1.5, seq(1.5, 6, 0.01), 6),
#         y = c(0, dnorm(seq(1.5, 6, 0.01)), 0), 
#         col = "steelblue")
ggplot() +
  stat_function(fun = dnorm, xlim = c(-4, 4)) +
  scale_x_continuous(breaks = c(-1.5, 0, 1.5),
                     labels = c(expression(-frac(bar(Y)^"act"~-~bar(mu)["Y,0"], sigma[bar(Y)])),
                                0,
                                expression(frac(bar(Y)^"act"~-~bar(mu)["Y,0"], sigma[bar(Y)]))),
                     limits = c(-4, 4)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_polygon(aes(x = c(-6, seq(-6, -1.5, 0.01), -1.5),
                   y = c(0, dnorm(seq(-6, -1.5, 0.01)), 0)),
               fill = "steelblue") +
  geom_polygon(aes(x = c(1.5, seq(1.5, 6, 0.01), 6),
                   y = c(0, dnorm(seq(1.5, 6, 0.01)), 0)),
               fill = "steelblue") +
  labs(x = "z", y = "", title = "Calculating a p-Value") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.x = element_line(),
        rect = element_blank(), plot.title = element_text(hjust = 0.5))


# vector of sample sizes
n <- c(10000, 5000, 2000, 1000, 500)

# sample observations, estimate using 'sd()' and plot the estimated distributions
sq_y <- replicate(n = 10000, expr = sd(rnorm(n[1], 10, 3)))
plot(density(sq_y),
     main = expression("Sampling Distributions of" ~ s[Y]),
     xlab = expression(s[y]),
     lwd = 2)
# 
# for (i in 2:length(n)) {
#     sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i], 10, 3)))
#     lines(density(sq_y),
#           col = i,
#           lwd = 2)
# }

# add a legend
# legend("topleft",
#        legend = c(expression(n == 10000),
#                   expression(n == 5000),
#                   expression(n == 2000),
#                   expression(n == 1000),
#                   expression(n == 500)),
#        col = 1:5,
#        lwd = 2)
sq_y_df <- tibble(sq_y = numeric(), n = numeric())
for (i in 1:length(n)) {
    sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i], 10, 3)))
    sq_y_df <- sq_y_df %>% 
      add_row(sq_y = sq_y, n = n[i])
}

ggplot(sq_y_df, aes(x = sq_y, color = factor(n))) +
  stat_density(geom = "line", position = "identity") +
  labs(x = expression(s[y]), y = "Density",
       title = expression("Sampling Distributions of" ~ s[Y])) +
  scale_color_manual(name = NULL,
                     values = c("10000" = "black",
                                  "5000" = "red2",
                                  "2000" = "green",
                                  "1000" = "steelblue",
                                  "500" = "orange"),
                     labels = c("10000" = expression(n == 10000),
                                "5000" = expression(n == 5000),
                                "2000" = expression(n == 2000),
                                "1000" = expression(n == 1000),
                                "500" = expression(n == 500))) + 
  xlim(c(2.9, 3.1)) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# draw 10000 samples of size 100 and estimate the mean of Y and
# estimate the standard error of the sample mean
set.seed(1)
mean_estimates <- numeric(10000)
se_estimates <- numeric(10000)

for (i in 1:10000) {
    
    s <- sample(0:1, 
                size = 100,  
                prob = c(0.9, 0.1),
                replace = T)
    
    mean_estimates[i] <- mean(s)
    se_estimates[i] <- sqrt(mean(s) * (1 - mean(s)) / 100)
    
}

mean(mean_estimates)
#> [1] 0.099553
mean(se_estimates)
#> [1] 0.0294954


# sample and estimate, compute standard error
set.seed(1)
samplemean_act <- mean(
    sample(0:1, 
           prob = c(0.9, 0.1), 
           replace = T, 
           size = 100))

SE_samplemean <- sqrt(samplemean_act * (1 - samplemean_act) / 100)

# null hypothesis
mean_h0 <- 0.1

# compute the p-value
pvalue <- 2 * pnorm(- abs(samplemean_act - mean_h0) / SE_samplemean)
pvalue
#> [1] 0.09212296


# compute a t-statistic for the sample mean
tstatistic <- (samplemean_act - mean_h0) / SE_samplemean
tstatistic
#> [1] -1.684304


# prepare empty vector for t-statistics
tstatistics <- numeric(10000)

# set sample size
n <- 300

# simulate 10000 t-statistics
for (i in 1:10000) {
    
    s <- sample(0:1, 
                size = n,  
                prob = c(0.9, 0.1),
                replace = T)
    
    tstatistics[i] <- (mean(s)-0.1)/sqrt(var(s)/n)
    
}


# plot density and compare to N(0,1) density
# plot(density(tstatistics),
#      xlab = "t-statistic",
#      main = "Estimated Distribution of the t-statistic when n=300",
#      lwd = 2,
#      xlim = c(-4, 4),
#      col = "steelblue")

# N(0,1) density (dashed)
# curve(dnorm(x), 
#       add = T, 
#       lty = 2, 
#       lwd = 2)
ggplot() +
  stat_function(fun = dnorm, color = "black", linetype = "dashed") +
  stat_density(aes(x = tstatistics), geom = "line", color = "steelblue") +
  labs(x = "t-statistic", y = "Density",
       title = "Estimated Distribution of the t-statistic when n=300") +
  xlim(c(-4, 4)) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# check whether p-value < 0.05
pvalue < 0.05
#> [1] FALSE


# check the critical value
qnorm(p = 0.975)
#> [1] 1.959964

# check whether the null is rejected using the t-statistic computed further above
abs(tstatistic) > 1.96
#> [1] FALSE


# plot the standard normal density on the domain [-4,4]
# curve(dnorm(x),
#       xlim = c(-4, 4),
#       main = "Rejection Region of a Right-Sided Test",
#       yaxs = "i",
#       xlab = "t-statistic",
#       ylab = "",
#       lwd = 2,
#       axes = "F")

# add the x-axis
# axis(1, 
#      at = c(-4, 0, 1.64, 4), 
#      padj = 0.5,
#      labels = c("", 0, expression(Phi^-1~(.95)==1.64), ""))

# shade the rejection region in the left tail
# polygon(x = c(1.64, seq(1.64, 4, 0.01), 4),
#         y = c(0, dnorm(seq(1.64, 4, 0.01)), 0), 
#         col = "darkred")
ggplot() +
  stat_function(fun = dnorm, xlim = c(-4, 4)) +
  scale_x_continuous(breaks = c(-4, 0, 1.64, 4),
                     labels = c("", 0, expression(Phi^-1~(.95)==1.64), "")) +
  scale_y_continuous(expand = c(0,0)) +
  geom_polygon(aes(x = c(1.64, seq(1.64, 4, 0.01), 4),
                   y = c(0, dnorm(seq(1.64, 4, 0.01)), 0)),
               fill = "darkred") +
  labs(x = "t-statistic", y = "", title = "Rejection Region of a Right-Sided Test") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.x = element_line(),
        rect = element_blank(), plot.title = element_text(hjust = 0.5))


# plot the the standard normal density on the domain [-4,4]
# curve(dnorm(x),
#       xlim = c(-4, 4),
#       main = "Rejection Region of a Left-Sided Test",
#       yaxs = "i",
#       xlab = "t-statistic",
#       ylab = "",
#       lwd = 2,
#       axes = "F")

# add x-axis
# axis(1, 
#      at = c(-4, 0, -1.64, 4), 
#      padj = 0.5,
#      labels = c("", 0, expression(Phi^-1~(.05)==-1.64), ""))

# shade rejection region in right tail
# polygon(x = c(-4, seq(-4, -1.64, 0.01), -1.64),
#         y = c(0, dnorm(seq(-4, -1.64, 0.01)), 0), 
#         col = "darkred")
ggplot() +
  stat_function(fun = dnorm, xlim = c(-4, 4)) +
  scale_x_continuous(breaks = c(-4, 0, -1.64, 4),
                     labels = c("", 0, expression(Phi^-1~(.05)==-1.64), "")) +
  scale_y_continuous(expand = c(0,0)) +
  geom_polygon(aes(x = c(-4, seq(-4, -1.64, 0.01), -1.64),
                   y = c(0, dnorm(seq(-4, -1.64, 0.01)), 0)),
               fill = "darkred") +
  labs(x = "t-statistic", y = "", title = "Rejection Region of a Left-Sided Test") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), axis.line.x = element_line(),
        rect = element_blank(), plot.title = element_text(hjust = 0.5))


# 3.4 Confidence Intervals for the Population Mean -----------------------------

# set seed
set.seed(1)

# generate some sample data
sampledata <- rnorm(100, 10, 10)

# check the type of the outcome produced by t.test
typeof(t.test(sampledata))
#> [1] "list"

# display the list elements produced by t.test
ls(t.test(sampledata))
#>  [1] "alternative" "conf.int"    "data.name"   "estimate"    "method"     
#>  [6] "null.value"  "p.value"     "parameter"   "statistic"   "stderr"


t.test(sampledata)$"conf.int"
#> [1]  9.306651 12.871096
#> attr(,"conf.level")
#> [1] 0.95


t.test(sampledata)
#> 
#>  One Sample t-test
#> 
#> data:  sampledata
#> t = 12.346, df = 99, p-value < 2.2e-16
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>   9.306651 12.871096
#> sample estimates:
#> mean of x 
#>  11.08887


# 3.5 Comparing Means from Different Populations -------------------------------

# set random seed
set.seed(1)

# draw data from two different populations with equal mean
sample_pop1 <- rnorm(100, 10, 10)
sample_pop2 <- rnorm(100, 10, 20)

# perform a two sample t-test
t.test(sample_pop1, sample_pop2)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  sample_pop1 and sample_pop2
#> t = 0.872, df = 140.52, p-value = 0.3847
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -2.338012  6.028083
#> sample estimates:
#> mean of x mean of y 
#> 11.088874  9.243838


# 3.6 An Application to the Gender Gap of Earnings -----------------------------

# import the data into R
cps <- read_excel(path = "data/cps_ch3.xlsx")


# get an overview of the data structure
head(cps)
#> # A tibble: 6 × 3
#>   a_sex  year ahe08
#>   <dbl> <dbl> <dbl>
#> 1     1  1992  17.2
#> 2     1  1992  15.3
#> 3     1  1992  22.9
#> 4     2  1992  13.3
#> 5     1  1992  22.1
#> 6     2  1992  12.2

# group data by gender and year and compute the mean, standard deviation
# and number of observations for each group
avgs <- cps %>% 
    group_by(a_sex, year) %>% 
    summarise(mean(ahe08), 
              sd(ahe08), 
              n())

# print the results to the console
print(avgs)
#> # A tibble: 10 × 5
#> # Groups:   a_sex [2]
#>    a_sex  year `mean(ahe08)` `sd(ahe08)` `n()`
#>    <dbl> <dbl>         <dbl>       <dbl> <int>
#>  1     1  1992          23.3       10.2   1594
#>  2     1  1996          22.5       10.1   1379
#>  3     1  2000          24.9       11.6   1303
#>  4     1  2004          25.1       12.0   1894
#>  5     1  2008          25.0       11.8   1838
#>  6     2  1992          20.0        7.87  1368
#>  7     2  1996          19.0        7.95  1230
#>  8     2  2000          20.7        9.36  1181
#>  9     2  2004          21.0        9.36  1735
#> 10     2  2008          20.9        9.66  1871


# split the dataset by gender
male <- avgs %>% filter(a_sex == 1) 

female <- avgs %>% filter(a_sex == 2)

# rename columns of both splits
colnames(male)   <- c("Sex", "Year", "Y_bar_m", "s_m", "n_m")
colnames(female) <- c("Sex", "Year", "Y_bar_f", "s_f", "n_f")

# estimate gender gaps
gap <- male$Y_bar_m - female$Y_bar_f

# compute standard errors
gap_se <- sqrt(male$s_m^2 / male$n_m + female$s_f^2 / female$n_f)

#Compute confidence intervals for all dates
gap_ci_l <- gap - 1.96 * gap_se

gap_ci_u <- gap + 1.96 * gap_se

result <- cbind(male[,-1], female[,-(1:2)], gap, gap_se, gap_ci_l, gap_ci_u)

# print the results to the console
print(result, digits = 3)
#>   Year Y_bar_m  s_m  n_m Y_bar_f  s_f  n_f  gap gap_se gap_ci_l gap_ci_u
#> 1 1992    23.3 10.2 1594    20.0 7.87 1368 3.23  0.332     2.58     3.88
#> 2 1996    22.5 10.1 1379    19.0 7.95 1230 3.49  0.354     2.80     4.19
#> 3 2000    24.9 11.6 1303    20.7 9.36 1181 4.14  0.421     3.32     4.97
#> 4 2004    25.1 12.0 1894    21.0 9.36 1735 4.10  0.356     3.40     4.80
#> 5 2008    25.0 11.8 1838    20.9 9.66 1871 4.10  0.354     3.41     4.80


# 3.7 Scatterplots, Sample Covariance and Sample Correlation -------------------

# set random seed
set.seed(123)

# generate dataset
X <- runif(n = 100, 
           min = 18, 
           max = 70)

Y <- X + rnorm(n=100, 50, 15)

# plot observations
# plot(X, 
#      Y, 
#      type = "p",
#      main = "A Scatterplot of X and Y",
#      xlab = "Age",
#      ylab = "Earnings",
#      col = "steelblue",
#      pch = 19)
ggplot(tibble(X, Y), aes(X, Y)) +
  geom_point() +
  labs(x = "Age", y = "Earnings", title = "A Scatterplot of X and Y")


# compute sample covariance of X and Y
cov(X, Y)
#> [1] 213.934

# compute sample correlation between X and Y
cor(X, Y)
#> [1] 0.706372

# an equivalent way to compute the sample correlation
cov(X, Y) / (sd(X) * sd(Y))
#> [1] 0.706372


# set random seed
set.seed(1)

# positive correlation (0.81)
example1 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, 2, 2, 3), ncol = 2),
                    empirical = TRUE)

# negative correlation (-0.81)
example2 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, -2, -2, 3), ncol = 2),
                    empirical = TRUE)

# no correlation 
example3 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(1, 0, 0, 1), ncol = 2),
                    empirical = TRUE)

# no correlation (quadratic relationship)
X <- seq(-3, 3, 0.01)
Y <- - X^2 + rnorm(length(X))

example4 <- cbind(X, Y)

# divide plot area as 2-by-2 array
# par(mfrow = c(2, 2))

# plot datasets
# plot(example1, col = "steelblue", pch = 20, xlab = "X", ylab = "Y", 
#      main = "Correlation = 0.81")
# 
# plot(example2, col = "steelblue", pch = 20, xlab = "X", ylab = "Y", 
#      main = "Correlation = -0.81")
# 
# plot(example3, col = "steelblue", pch = 20, xlab = "X", ylab = "Y", 
#      main = "Correlation = 0")
# 
# plot(example4, col = "steelblue", pch = 20, xlab = "X", ylab = "Y", 
#      main = "Correlation = 0")
p1 <- ggplot(as_tibble(example1)) +
  geom_point(aes(V1, V2)) +
  labs(x = "X", y = "Y", title = "Correlation = 0.81")
p2 <- ggplot(as_tibble(example2)) +
  geom_point(aes(V1, V2)) +
  labs(x = "X", y = "Y", title = "Correlation = -0.81")
p3 <- ggplot(as_tibble(example3)) +
  geom_point(aes(V1, V2)) +
  labs(x = "X", y = "Y", title = "Correlation = 0")
p4 <- ggplot(as_tibble(example4)) +
  geom_point(aes(X, Y)) +
  labs(x = "X", y = "Y", title = "Correlation = 0")
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)


# 3.8 Exercises ----------------------------------------------------------------

# 1. Biased …
# define a function for the estimator
Y_tilde <- function(x) {
    sum(x) / (length(x) - 1)
}

# repeatedly compute estimates and store the results in est_biased
set.seed(123)
est_biased <- replicate(n = 10000, 
                        expr = Y_tilde(rnorm(5, 10, 25)))

# plot a histogram of est_biased
# hist(est_biased,
#      breaks = 15,
#      main = "Histogram of Estimates",
#      xlab = "Estimates",
#      ylab = "Frequency",
#      col = "steelblue")

# add a red vertical line at mu = 10
# abline(v = 10,
#        col = "red2",
#        lwd = 2)
ggplot(tibble(est_biased), aes(est_biased)) +
  geom_histogram(bins = 15, fill = "steelblue") +
  geom_vline(xintercept = 10, color = "red2", linetype = "dashed") +
  labs(x = "Estimates", y = "Frequency",
       title = "Histogram of Estimates") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# 2. … but consistent estimator
# compute repeatedly estimates and store the results in est_consistent
set.seed(123)
est_consistent <- replicate(n = 10000, 
                        expr = Y_tilde(rnorm(1000, 10, 25)))

# plot a histogram of est_consistent
# hist(est_consistent,
#      breaks = 15,
#      main = "Histogram of Estimates",
#      xlab = "Estimates",
#      ylab = "Frequency",
#      col = "steelblue")

# add a red vertical line at mu = 10
# abline(v = 10,
#        col = "red2",
#        lwd = 2)
ggplot(tibble(est_consistent), aes(est_consistent)) +
    geom_histogram(bins = 15, fill = "steelblue") +
    geom_vline(xintercept = 10, color = "red2", linetype = "dashed") +
    labs(x = "Estimates", y = "Frequency",
         title = "Histogram of Estimates") +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# 3. Efficiency of an Estimator
# verify that the alternative estimator is unbiased
n <- 100
w <- c(rep((1+0.5)/n, n/2), rep((1-0.5)/n, n/2))


# define the alternative estimator mu_tilde
mu_tilde <- function(x) {
    sum(x * w)
}


# compute repeatedly estimates for both estimators and store the results in est_bar and est_tilde
set.seed(123)

# compute the sample variances for est_bar and est_tilde
est_bar <- replicate(n = 10000, 
                     expr = mean(rnorm(n, 100, 50)))
est_tilde <- replicate(n = 10000, 
                       expr = mu_tilde(rnorm(n, 100, 50)))
var(est_bar)
#> [1] 24.88439

var(est_tilde)
#> [1] 31.22373


# 4. Hypothesis Test -  t-statistic
cps <- read_excel(path = "data/Exercises_CH03/cps12.xlsx")

# compute the t statistic by hand and assign it to tstat
tstat <- (mean(cps$ahe12)-23.5)/(sd(cps$ahe12)/sqrt(length(cps$ahe12)))
tstat
#> [1] 3.739356

# use tstat to accept or reject the null
tstat > qnorm(0.95)
#> [1] TRUE


# 5. Hypothesis Test - p-value
# compute the p-value by hand and assign it to pval
pval <- 1 - pnorm(tstat)
pval
#> [1] 9.224597e-05

# use pval to accept or reject the null
pval < 0.05
#> TRUE


# 6. Hypothesis Test - One Sample
# conduct the hypothesis test from the previous exercises with t.test()
t.test(cps$ahe12, mu = 23.5, alternative = "greater")
#>     One Sample t-test
#> 	
#> data:  cps$ahe12
#> t = 3.7394, df = 19349, p-value = 9.251e-05
#> alternative hypothesis: true mean is greater than 23.5
#> 95 percent confidence interval:
#>  23.66779      Inf
#> sample estimates:
#> mean of x 
#>  23.79957

# extract t statistic and p-value from the list created by t.test()
t.test(cps$ahe12, mu = 23.5, alternative = "greater")$statistic
#>        t
#> 3.739356
t.test(cps$ahe12, mu = 23.5, alternative = "greater")$p.value
#> [1] 9.251185e-05

# verify that using the normal approximation is valid here as well
t.test(cps$ahe12, mu = 23.5, alternative = "greater")$p.value - pval
#> [1] 2.658761e-07


# 7. Hypothesis Test - Two Sample t-test
por_fre <- read_csv(file = "data/Exercises_CH03/por_fre.csv")
portpirie <- por_fre$portpirie
fremantle <- por_fre$fremantle

t.test(portpirie, fremantle)
#> 	       Welch Two Sample t-test
#>
#> data:  portpirie and fremantle
#> t = 43.727, df = 47.347, p-value < 2.2e-16
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  2.584598 2.833833
#> sample estimates:
#> mean of x mean of y 
#> 4.172400  1.463185 


# 8. Confidence Interval
# construct a 95%-confidence interval using t.test()
t.test(cps$ahe12, mu = 23.5, alternative = "greater")$conf.int
#> [1] 23.66779     Inf
#> attr(,"conf.level")
#> [1] 0.95


# 9. (Co)variance and Correlation I
XY01 <- read_csv("data/Exercises_CH03/XY01.csv")
X <- XY01$X
Y <- XY01$Y

# compute the variance of X
cov(X, X)
#> [1] 898.1355
cov(X, X) == var(X)
#> [1] TRUE

# compute the covariance of X and Y
covXY <- cov(X, Y)
covXY
#> [1] 2850.209

# compute the correlation between X and Y
cor(X, Y)
#> [1] 0.7039294


# 10. (Co)variance and Correlation II
XY02 <- read_csv("data/Exercises_CH03/XY02.csv")
X <- XY02$X
Y <- XY02$Y

ggplot() +
  geom_point(aes(X, Y)) +
  labs(x = "X", y = "Y", title = "Scatterplot of X and Y")

# compute the correlation between X and Y
cor(X, Y)
#> [1] -0.9154376

# linear regression
ggplot() +
  geom_point(aes(X, Y)) +
  geom_smooth(aes(X, Y), method = "lm") +
  labs(x = "X", y = "Y", title = "Scatterplot of X and Y") 

# non-linear regression
ggplot() +
  geom_point(aes(X, Y)) +
  geom_smooth(aes(X, Y), method = "lm", formula = y ~ poly(x, 2)) +
  labs(x = "X", y = "Y", title = "Scatterplot of X and Y")