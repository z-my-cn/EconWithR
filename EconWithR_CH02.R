# Econometrics with R
# Chapter 02 Probability Theory ------------------------------------------------

library(tidyverse)
library(ggplot2)
library(patchwork)

# 2.1 Random Variables and Probability Distributions ---------------------------

set.seed(123)
sample(1:6, size=1) 
#> [1] 3


# generate the vector of probabilities 
probability <- rep(1/6, 6) 

# plot the probabilities 
# plot(probability,
#      xlab = "Outcomes",
#      ylab="Probability",
#      main = "Probability Distribution",
#      pch=20) 
ggplot(tibble(probability), aes(x=1:6, y=probability)) +
  geom_point() +
  labs(x="Outcomes", y="Probability", 
       title="Probability Distribution")


# generate the vector of cumulative probabilities 
cum_probability <- cumsum(probability) 

# plot the probabilites 
# plot(cum_probability, 
#      xlab = "Outcomes", 
#      ylab="Cumulative Probability",
#      main = "Cumulative Probability Distribution",
#      pch=20) 
ggplot(tibble(cum_probability), aes(x=1:6, y=cum_probability)) +
  geom_point() +
  labs(x="Outcomes", y="Cumulative Probability", 
      title="Cumulative Probability Distribution")

set.seed(123)
sample(c("H", "T"), 1) 
#> [1] "H"


dbinom(x = 5,
       size = 10,
       prob = 0.5) 
#> [1] 0.2460938


# compute P(4 <= k <= 7) using 'dbinom()'
sum(dbinom(x = 4:7, size = 10, prob = 0.5))
#> [1] 0.7734375


# compute P(4 <= k <= 7) using 'pbinom()'
pbinom(size = 10, prob = 0.5, q = 7) - pbinom(size = 10, prob = 0.5, q = 3) 
#> [1] 0.7734375


# set up vector of possible outcomes
k <- 0:10
k
#>  [1]  0  1  2  3  4  5  6  7  8  9 10


# assign the probabilities
probability <- dbinom(x = k,
                      size = 10, 
                      prob = 0.5)

# plot the outcomes against their probabilities
# plot(x = k, 
#      y = probability,
#      ylab="Probability",
#      main = "Probability Distribution Function",
#      pch=20) 
ggplot(tibble(probability), aes(x=k, y=probability)) +
  geom_point() +
  labs(x="Outcomes", y="Probability", 
       title="Probability Distribution Function")


# compute cumulative probabilities
prob <- pbinom(q = k, 
               size = 10, 
               prob = 0.5)

# plot the cumulative probabilities
# plot(x = k, 
#      y = prob,
#      ylab="Probability",
#      main = "Cumulative Distribution Function",
#      pch=20) 
ggplot(tibble(prob), aes(x=k, y=prob)) +
  geom_point() +
  labs(x="Outcomes", y="Probability", 
       title="Cumulative Distribution Function")


# compute mean of natural numbers from 1 to 6
mean(1:6)
#> [1] 3.5


# set seed for reproducibility
set.seed(1)

# rolling a dice three times in a row
sample(1:6, 3, replace = T)
#> [1] 1 4 1


# set seed for reproducibility
set.seed(1)

# compute the sample mean of 10000 dice rolls
mean(sample(1:6, 
            10000, 
            replace = T))
#> [1] 3.5138


var(1:6)
#> [1] 3.5


# define functions
f <- function(x) 3 / x^4
g <- function(x) x * f(x)
h <- function(x) x^2 * f(x)


# compute area under the density curve
area <- integrate(f, 
                  lower = 1, 
                  upper = Inf)$value
area 
#> [1] 1

# compute E(X)
EX <- integrate(g,
                lower = 1,
                upper = Inf)$value
EX
#> [1] 1.5

# compute Var(X)
VarX <- integrate(h,
                  lower = 1,
                  upper = Inf)$value - EX^2 
VarX
#> [1] 0.75


# draw a plot of the N(0,1) PDF
# curve(dnorm(x),
#       xlim = c(-3.5, 3.5),
#       ylab = "Density", 
#       main = "Standard Normal Density Function") 
ggplot() +
  stat_function(fun = dnorm, xlim = c(-3.5, 3.5)) +
  labs(x="x", y="Density", 
       title="Standard Normal Density Function")


# compute density at x=-1.96, x=0 and x=1.96
dnorm(x = c(-1.96, 0, 1.96))
#> [1] 0.05844094 0.39894228 0.05844094

# plot the standard normal CDF
# curve(pnorm(x), 
#       xlim = c(-3.5, 3.5), 
#       ylab = "Probability", 
#       main = "Standard Normal Cumulative Distribution Function")
ggplot() +
  stat_function(fun = pnorm, xlim = c(-3.5, 3.5)) +
  labs(x="x", y="Probability", 
       title="Standard Normal Cumulative Distribution Function")


# define the standard normal PDF as an R function
f <- function(x) {
    1/(sqrt(2 * pi)) * exp(-0.5 * x^2)
}

# define a vector of reals
quants <- c(-1.96, 0, 1.96)

# compute densities
f(quants)
#> [1] 0.05844094 0.39894228 0.05844094

# compare to the results produced by 'dnorm()'
f(quants) == dnorm(quants)
#> [1] TRUE TRUE TRUE


# integrate f()
integrate(f, 
          lower = -Inf, 
          upper = 1.337)
#> 0.9093887 with absolute error < 1.7e-07


# compute the probability using pnorm()
pnorm(1.337)
#> [1] 0.9093887


# compute the probability
1 - 2 * (pnorm(-1.96)) 
#> [1] 0.9500042


pnorm(4, mean = 5, sd = 5) - pnorm(3, mean = 5, sd = 5) 
#> [1] 0.07616203


# plot the PDF
# curve(dchisq(x, df = 3), 
#       xlim = c(0, 10), 
#       ylim = c(0, 1), 
#       col = "blue",
#       ylab = "",
#       main = "p.d.f. and c.d.f of Chi-Squared Distribution, M = 3")

# add the CDF to the plot
# curve(pchisq(x, df = 3), 
#       xlim = c(0, 10), 
#       add = TRUE, 
#       col = "red")

# add a legend to the plot
# legend("topleft", 
#        c("PDF", "CDF"), 
#        col = c("blue", "red"), 
#        lty = c(1, 1))

ggplot() +
    stat_function(fun = dchisq, args = list(df=3), xlim = c(0, 10),
                  aes(color="PDF")) +
    stat_function(fun = pchisq, args = list(df=3), xlim = c(0, 10),
                  aes(color="CDF")) +
    scale_color_manual(values = c("PDF" = "blue", "CDF" = "red")) +
    labs(x="x", y="Probability", 
         title="p.d.f. and c.d.f of Chi-Squared Distribution, M = 3",
         color = NULL) +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))


# plot the density for M=1
# curve(dchisq(x, df = 1), 
#       xlim = c(0, 15), 
#       xlab = "x", 
#       ylab = "Density", 
#       main = "Chi-Square Distributed Random Variables")
# 
# add densities for M=2,...,7 to the plot using a 'for()' loop 
# for (M in 2:7) {
#     curve(dchisq(x, df = M),
#           xlim = c(0, 15), 
#           add = T, 
#           col = M)
# }
# 
# add a legend
# legend("topright", 
#        as.character(1:7), 
#        col = 1:7 , 
#        lty = 1, 
#        title = "D.F.")
df_chisq <- tibble(x=rep(seq(0, 15, 0.01), 7), M=rep(1:7, each=1501))
df_chisq <- df_chisq %>% 
    mutate(pdf = dchisq(x, df=M), D.F. = as.factor(M))

ggplot(df_chisq) +
    geom_line(aes(x=x, y=pdf, color=D.F.)) +
    scale_color_manual(values = c("1" = "black", "2" = "red", "3" = "green", 
                                  "4" = "blue",  "5" = "orange", "6" = "purple", 
                                  "7" = "brown")) +
    labs(x="x", y="Density", 
         title="Chi-Square Distributed Random Variables") +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
    ylim(0, 1)


# plot the standard normal density
# curve(dnorm(x), 
#       xlim = c(-4, 4), 
#       xlab = "x", 
#       lty = 2, 
#       ylab = "Density", 
#       main = "Densities of t Distributions")

# plot the t density for M=2
# curve(dt(x, df = 2), 
#       xlim = c(-4, 4), 
#       col = 2, 
#       add = T)

# plot the t density for M=4
# curve(dt(x, df = 4), 
#       xlim = c(-4, 4), 
#       col = 3, 
#       add = T)

# plot the t density for M=25
# curve(dt(x, df = 25), 
#       xlim = c(-4, 4), 
#       col = 4, 
#       add = T)

# add a legend
# legend("topright", 
#        c("N(0, 1)", "M=2", "M=4", "M=25"), 
#        col = 1:4, 
#        lty = c(2, 1, 1, 1))
df_t <- tibble(x=seq(-4, 4, 0.01), N01 = dnorm(x),
               M2 = dt(x, df=2), M4 = dt(x, df=4), M25 = dt(x, df=25))

df_t <- df_t %>% 
    pivot_longer(cols = c("N01", "M2", "M4", "M25"), 
                 names_to = "distribution", values_to = "Density")

ggplot(df_t) +
    geom_line(aes(x=x, y=Density, color=distribution, linetype=distribution)) +
    scale_color_manual(breaks = c("N01", "M2", "M4", "M25"),
                       values = c("N01" = "black", "M2" = "red", 
                                  "M4" = "green", "M25" = "blue")) +
    scale_linetype_manual(breaks = c("N01", "M2", "M4", "M25"), 
                          values = c("N01" = "dashed", "M2" = "solid", 
                                     "M4" = "solid", "M25" = "solid")) +
    labs(x="x", y="Density",
         title="Densities of t Distributions", 
         color = NULL, linetype = NULL) +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))


pf(2, df1 = 3, df2 = 14, lower.tail = F)
#> [1] 0.1603538


# define coordinate vectors for vertices of the polygon
# x <- c(2, seq(2, 10, 0.01), 10)
# y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
# curve(df(x ,3 ,14), 
#       ylim = c(0, 0.8), 
#       xlim = c(0, 10), 
#       ylab = "Density",
#       main = "Density Function")

# draw the polygon
# polygon(x, y, col = "orange")
df_F314 <- tibble(x = c(2, seq(2, 10, 0.01), 10), 
                  pdf = c(0, df(seq(2, 10, 0.01), 3, 14), 0))

ggplot() +
    stat_function(fun = df, args = list(df1=3, df2=14), xlim = c(0, 10),
                  color = "black") +
    geom_polygon(aes(x=x, y=pdf), fill="orange", data = df_F314) +
    labs(x="x", y="Density", 
         title="Density Function") +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    ylim(0, 0.8)


# 2.2 Random Sampling and the Distribution of Sample Averages ------------------

set.seed(123)
sum(sample(1:6, 2, replace = T))
#> [1] 9


# Vector of outcomes
S <- 2:12

# Vector of probabilities
PS <- c(1:6, 5:1) / 36

# Expectation of S
ES <- sum(S * PS)
ES
#> [1] 7

# Variance of S
VarS <- sum((S - c(ES))^2 * PS)
VarS
#> [1] 5.833333


# divide the plotting area into one row with two columns
# par(mfrow = c(1, 2), cex.main=1)

# plot the distribution of S
# barplot(PS, 
#         ylim = c(0, 0.2), 
#         xlab = "S", 
#         ylab = "Probability", 
#         col = "steelblue", 
#         space = 0, 
#         main = "Sum of Two Dice Rolls")

# plot the distribution of D 
# probability <- rep(1/6, 6)
# names(probability) <- 1:6

# barplot(probability, 
#         ylim = c(0, 0.2), 
#         xlab = "D", 
#         col = "steelblue", 
#         space = 0, 
#         main = "Outcome of a Single Dice Roll")
# Vector of outcomes
S <- 2:12

# Vector of probabilities
PS <- c(1:6, 5:1) / 36
p1 <- ggplot(aes(x=S, y=PS), data=tibble(S, PS)) +
    geom_col(fill="steelblue", width = 0.95) +
    labs(x="S", y="Probability", 
         title="Sum of Two Dice Rolls") +
    scale_x_continuous(breaks = seq(2, 12, 2)) +
    ylim(0, 0.2)

probability <- rep(1/6, 6)
p2 <- ggplot(aes(x=1:6, y=probability), data=tibble(probability)) +
    geom_col(fill="steelblue", width = 0.95) +
    labs(x="D", y = NULL,
         title="Outcome of a Single Dice Roll") +
    ylim(0, 0.2)

p1 + p2


# set sample size and number of samples
n <- 10
reps <- 10000

# perform random sampling
set.seed(123)
samples <- replicate(reps, rnorm(n)) # 10 x 10000 sample matrix

# compute sample means
sample.avgs <- colMeans(samples)


# check that 'sample.avgs' is a vector
is.vector(sample.avgs) 
#> [1] TRUE

# print the first few entries to the console
head(sample.avgs)
#> [1]  0.074625644  0.208621961 -0.424558873  0.322044550 -0.008715537


# Plot the density histogram
# hist(sample.avgs, 
#      ylim = c(0, 1.4), 
#      col = "steelblue" , 
#      freq = F, 
#      breaks = 20,
#      main="Histogram of Sample Averages",
#      xlab="Sample Averages")
# 
# overlay the theoretical distribution of sample averages on top of the histogram
# curve(dnorm(x, sd = 1/sqrt(n)), 
#       col = "red", 
#       lwd = "2", 
#       add = T)
ggplot(tibble(sample.avgs), aes(x=sample.avgs)) +
    geom_histogram(aes(y=..density..), binwidth = 0.1, 
                   fill="steelblue", color = 'black') +
    geom_line(aes(x=sample.avgs, y=dnorm(sample.avgs, sd=1/sqrt(n))), 
              color="red", linewidth = 1) +
    labs(x="Sample Averages", y="Density", 
         title="Histogram of Sample Averages") +
    ylim(0, 1.4)


# number of repetitions
reps <- 10000

# set degrees of freedom of a chi-Square Distribution
DF <- 3 

# sample 10000 column vectors à 3 N(0,1) R.V.S
set.seed(123)
Z <- replicate(reps, rnorm(DF)) 

# column sums of squares
X <- colSums(Z^2)

# histogram of column sums of squares
# hist(X, 
#      freq = F, 
#      col = "steelblue", 
#      breaks = 40, 
#      ylab = "Density", 
#      main = "")

# add theoretical density
# curve(dchisq(x, df = DF), 
#       type = 'l', 
#       lwd = 2, 
#       col = "red", 
#       add = T)
ggplot(tibble(X), aes(x=X)) +
    geom_histogram(aes(y=..density..), bins = 40, 
                   fill="steelblue", color = 'black') +
    geom_line(aes(x=X, y=dchisq(X, df=DF)), 
              color="red", linewidth = 1) +
    labs(x="Sample Averages", y="Density", 
         title="Histogram of Sample Averages")


# set seed
set.seed(1)

# set number of coin tosses and simulate
N <- 30000
Y <- sample(0:1, N, replace = T)

# Calculate R_n for 1:N
S <- cumsum(Y)
R <- S/(1:N)

# Plot the path.
# plot(R, 
#      ylim = c(0.3, 0.7), 
#      type = "l", 
#      col = "steelblue", 
#      lwd = 2, 
#      xlab = "n", 
#      ylab = "R_n",
#      main = "Converging Share of Heads in Repeated Coin Tossing")

# Add a dashed line for R_n = 0.5
# lines(c(0, N), 
#       c(0.5, 0.5), 
#       col = "darkred", 
#       lty = 2, 
#       lwd = 1)
ggplot() +
    geom_line(aes(x=1:N, y=R), data = tibble(R), color="steelblue", 
              linewidth = 1) +
    geom_line(aes(x=c(0, N), y=c(0.5, 0.5)), 
              color="darkred", linetype = "dashed", linewidth = 1) +
    labs(x="n", y="R_n", 
         title="Converging Share of Heads in Repeated Coin Tossing") +
    ylim(0.3, 0.7)


# subdivide the plot panel into a 2-by-2 array
# par(mfrow = c(2, 2))

# set the number of repetitions and the sample sizes
reps <- 10000
sample.sizes <- c(5, 20, 75, 100)

# set seed for reproducibility
set.seed(123)

# outer loop (loop over the sample sizes)
# for (n in sample.sizes) {
#     
#     samplemean <- rep(0, reps) #initialize the vector of sample means
#     stdsamplemean <- rep(0, reps) #initialize the vector of standardized sample means
#     
#     # inner loop (loop over repetitions)   
#     for (i in 1:reps) {
#         x <- rbinom(n, 1, 0.5)
#         samplemean[i] <- mean(x)
#         stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/0.5
#     }
#     
#     # plot histogram and overlay the N(0,1) density in every iteration    
#     hist(stdsamplemean, 
#          col = "steelblue", 
#          freq = FALSE, 
#          breaks = 40,
#          xlim = c(-3, 3), 
#          ylim = c(0, 0.8), 
#          xlab = paste("n =", n), 
#          main = "")
#     
#     curve(dnorm(x), 
#           lwd = 2, 
#           col = "darkred", 
#           add = TRUE)
# }  
generate_data <- function(n) {
    tibble(
        sample_mean = replicate(reps, mean(rbinom(n, 1, 0.5))),
        std_sample_mean = sqrt(n) * (sample_mean - 0.5) / 0.5,
        n = factor(n) # factor for faceting by sample size
    )
}

# Generate data for each sample size and bind into one data frame
df_sample <- map_dfr(sample.sizes, generate_data)

# Plot using ggplot2
ggplot(df_sample, aes(x = std_sample_mean)) +
    geom_histogram(aes(y = ..density..), bins = 40, fill = "steelblue") +
    stat_function(fun = dnorm, color = "darkred", size = 1) +
    facet_wrap(~n, scales = "free", labeller = label_both) +
    labs(x = "Standardized Sample Mean", y = "Density") +
    theme_minimal()


# 2.3 Exercises ----------------------------------------------------------------


# 1. Sampling
# set seed for reproducibility
set.seed(123)

# draw the winning numbers for this week
sample(1:49, 6)
#> [1] 31 15 14  3 42 43


# 2. Probability Density Function
# define the PDF
f <- function(x) {
    x/4 * exp(-x^2/8)
}

# integrate f over the domain
integrate(f, lower = 0, upper = Inf)$value
#> [1] 1


# 3. Expected Value and Variance
# define the function ex
ex <- function(x) {
    x * f(x)
}

# compute the expected value of X
EX <- integrate(ex, lower = 0, upper = Inf)$value
EX
#> [1] 2.506628

# define the function ex2
ex2 <- function(x) {
    x^2 * f(x)
}

# compute the expected value of X^2
EX2 <- integrate(ex2, lower = 0, upper = Inf)$value
EX2
#> [1] 8

# compute the variance of X
VarX <- EX2 - EX^2
VarX
#> [1] 1.716815


# 4. Standard Normal Distribution I
# compute the value of the standard normal density at c=3
dnorm(3)
#> [1] 0.004431848


# 5. Standard Normal Distribution II
# compute the probability
pnorm(1.64) - pnorm(-1.64)
#> [1] 0.8989948


# 6. Normal Distribution I
# compute the 99% quantile of a normal distribution with mu = 5 and sigma^2 = 25.
qnorm(0.99, mean = 5, sd = 5)
#> [1] 16.63174


# 7. Normal Distribution II
set.seed(123)
# generate 10 random numbers from the given distribution.
rnorm(10, mean = 2, sd = sqrt(12))
#> [1]  0.05845541  1.20264179  7.39952399  2.24424823  2.44786585  7.94115939
#> [7]  3.59666057 -2.38230067 -0.37932807  0.45618165


# 8. Chi-squared Distribution I
# plot the PDF of a chi^2 random variable with df = 10
# curve(dchisq(x, df = 10),
#       xlim = c(0, 25),
#       ylab = "Density",
#       main = "Chi-squared Distribution, M = 10")
ggplot() +
    stat_function(fun = dchisq, args = list(df=10), xlim = c(0, 25)) +
    labs(x="x", y="Density", 
         title="Chi-squared Distribution, M = 10")


# 9. Chi-squared Distribution II
# X1 and X2 are independent normally distributed random variables with
# mu = 0 and sigma^2 = 15
# compute the probability
# P(X1^2 + X2^2 > 10)

# transform the problem to a chi^2 distribution with df = 2
# X1/sqrt(15) and X2/sqrt(15) are independent N(0,1) random variables
# X1^2/15 + X2^2/15 is a chi^2 random variable with df = 2

# compute the probability
1 - pchisq(10/15, df = 2)
#> [1] 0.7165313

# the same as
pchisq(10/15, df = 2, lower.tail = FALSE)
#> [1] 0.7165313


# 10. Student t Distribution I
# compute the 95% quantile of a t distribution with 10000 degrees of freedom
qt(0.95, df = 10000)
#> [1] 1.645006

# compute the 95% quantile of a standard normal distribution
qnorm(0.95)
#> [1] 1.644854

# Notice: the quantiles are very close to each other.


# 11. Student t Distribution II
set.seed(123)
# generate 1000 random numbers from the given distribution. Assign them to the variable x.
x <- rt(1000, df = 5)

# compute the sample mean of x.
mean(x)
#> [1] -0.06678201


# 12. F Distribution I
# plot the quantile function of the given distribution
# curve(qf(x, df1 = 10, df2 = 4),
#       xlim = c(0, 1),
#       ylab = "Quantile",
#       main = "F Distribution, M = 10, N = 4")
ggplot() +
    stat_function(fun = qf, args = list(df1=10, df2=4), xlim = c(0, 1)) +
    labs(x="x", y="Quantile", 
         title="F Distribution, M = 10, N = 4")


# 13. F Distribution II
# compute the probability by integration
integrate(df, lower = 1, upper = 10, df1 = 4, df2 = 5)$value
#> [1] 0.472397

