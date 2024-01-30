# Econometrics with R
# Chapter 05 Hypothesis Tests and Confidence Intervals in SLR Model ------------
library(AER)
library(tidyverse)
library(ggplot2)
library(latex2exp)

# 5.1 Testing Two-Sided Hypotheses Concerning the Slope Coefficient ------------

# load the `CASchools` dataset
data(CASchools)

# add student-teacher ratio
# CASchools$STR <- CASchools$students/CASchools$teachers

# add average test-score
# CASchools$score <- (CASchools$read + CASchools$math)/2

CASchools <- as_tibble(CASchools)
CASchools <- CASchools %>%
  mutate(STR = students/teachers,
         score = (read + math)/2)

# estimate the model
linear_model <- lm(score ~ STR, data = CASchools)          


# print the summary of the coefficients to the console
summary(linear_model)$coefficients
#>               Estimate Std. Error   t value      Pr(>|t|)
#> (Intercept) 698.932949  9.4674911 73.824516 6.569846e-242
#> STR          -2.279808  0.4798255 -4.751327  2.783308e-06


# determine residual degrees of freedom
linear_model$df.residual
#> [1] 418


2 * pt(-4.751327, df = 418)
#> [1] 2.78331e-06


2 * pnorm(-4.751327)
#> [1] 2.02086e-06


# Plot the standard normal on the support [-6,6]
# t <- seq(-6, 6, 0.01)

# plot(x = t, 
#      y = dnorm(t, 0, 1), 
#      type = "l", 
#      col = "steelblue", 
#      lwd = 2, 
#      yaxs = "i", 
#      axes = F, 
#      ylab = "", 
#      main = expression("Calculating the p-value of a Two-sided Test when"
#                        ~ t^act ~ "=-4.75"), 
#      cex.lab = 0.7,
#      cex.main = 1)

# tact <- -4.75

# axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)

# Shade the critical regions using polygon():

# critical region in left tail
# polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
#         y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
#         col = 'orange')

# critical region in right tail
# polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
#         y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
#         col = 'orange')

# Add arrows and texts indicating critical regions and the p-value
# arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
# arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
# 
# arrows(-5, 0.16, -4.75, 0, length = 0.1)
# arrows(5, 0.16, 4.75, 0, length = 0.1)
# 
# text(-3.5, 0.22, 
#      labels = expression("0.025"~"="~over(alpha, 2)),
#      cex = 0.7)
# text(3.5, 0.22, 
#      labels = expression("0.025"~"="~over(alpha, 2)),
#      cex = 0.7)
# 
# text(-5, 0.18, 
#      labels = expression(paste("-|",t[act],"|")), 
#      cex = 0.7)
# text(5, 0.18, 
#      labels = expression(paste("|",t[act],"|")), 
#      cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^act and -t^act 
# rug(c(-1.96, 1.96), ticksize  = 0.145, lwd = 2, col = "darkred")
# rug(c(-tact, tact), ticksize  = -0.0451, lwd = 2, col = "darkgreen")
tact <- -4.75
ggplot() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), xlim = c(-6, 6), 
                geom = "line", size = 1.2, color = "steelblue") +
  geom_polygon(aes(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
                   y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0)), 
               fill = "orange", alpha = 0.5) +
  geom_polygon(aes(x = c(1.96, seq(1.96, 6, 0.01), 6),
                   y = c(0, dnorm(seq(1.96, 6, 0.01)), 0)), 
               fill = "orange", alpha = 0.5) +
  geom_segment(aes(x = -3.5, y = 0.2, xend = -2.5, yend = 0.02), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = 3.5, y = 0.2, xend = 2.5, yend = 0.02), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = -5, y = 0.16, xend = -4.75, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_segment(aes(x = 5, y = 0.16, xend = 4.75, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  geom_text(aes(x = -3.5, y = 0.22), parse = TRUE,
            label = "0.025 == frac(alpha, 2)") +
  geom_text(aes(x = 3.5, y = 0.22), parse = TRUE, 
            label = "0.025 == frac(alpha, 2)") +
  geom_text(aes(x = -5, y = 0.18), parse = TRUE, label = "-abs(t[act])") +
  geom_text(aes(x = 5, y = 0.18), parse = TRUE, label = "abs(t[act])") +
  geom_rug(aes(x = c(-1.96, 1.96)), 
           sides = "b", length = unit(0.145, "cm"), color = "darkred") +
  geom_rug(aes(x = c(-tact, tact)),
           sides = "b", length = unit(-0.0451, "cm"), color = "darkgreen") +
  
  scale_x_continuous(breaks = c(0, -1.96, 1.96, -tact, tact),
                     limits = c(-6, 6)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(t[act]), y = "",
       title = expression("Calculating the p-value of a Two-sided Test when"
                           ~ t^act ~ "=-4.75")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")


# 5.2 Confidence Intervals for Regression Coefficients -------------------------

# set seed for reproducibility
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)

# plot(Y, 
#      pch = 19, 
#      col = "steelblue")
ggplot() +
  geom_point(aes(x = 1:100, y = Y), 
             color = "steelblue", size = 2) +
  labs(x = "Observation", y = "Y", title = "Sample Data")


cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10)
#>       CIlower  CIupper
#> [1,] 4.502625 6.462625


# set seed
set.seed(1)

# initialize vectors of lower and upper interval boundaries
lower <- numeric(10000)
upper <- numeric(10000)

# loop sampling / estimation / CI
for(i in 1:10000) {
    
    Y <- rnorm(100, mean = 5, sd = 5)
    lower[i] <- mean(Y) - 1.96 * 5 / 10
    upper[i] <- mean(Y) + 1.96 * 5 / 10
    
}

# join vectors of interval bounds in a matrix
CIs <- cbind(lower, upper)


mean(CIs[, 1] <= 5 & 5 <= CIs[, 2])
#> [1] 0.9487


# identify intervals not covering mu
# (4 intervals out of 100)
# ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

# initialize the plot
# plot(0, 
#      xlim = c(3, 7), 
#      ylim = c(1, 100), 
#      ylab = "Sample", 
#      xlab = expression(mu), 
#      main = "Confidence Intervals")

# set up color vector
# colors <- rep(gray(0.6), 100)
# colors[ID] <- "red"

# draw reference line at mu=5
# abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
# for(j in 1:100) {
#     
#     lines(c(CIs[j, 1], CIs[j, 2]), 
#           c(j, j), 
#           col = colors[j], 
#           lwd = 2)
#     
# }
df <- tibble(ID = 1:100,
             lower = CIs[1:100, 1],
             upper = CIs[1:100, 2]) %>%
  mutate(color = ifelse(lower <= 5 & 5 <= upper, "gray60", "red"))

ggplot(df) +
  geom_segment(aes(x = lower, xend = upper, y = ID, yend = ID, color = color), 
               size = 1) +
  geom_vline(aes(xintercept = 5), linetype = "dashed") +
  scale_color_identity() +
  xlim(2.8, 7.2) +
  labs(x = expression(mu), y = "Sample", title = "Confidence Intervals") +
  theme(legend.position = "none")


# compute 95% confidence interval for coefficients in 'linear_model'
confint(linear_model)
#>                 2.5 %     97.5 %
#> (Intercept) 680.32312 717.542775
#> STR          -3.22298  -1.336636


# compute 95% confidence interval for coefficients in 'linear_model' by hand
lm_sum <- summary(linear_model)

c("lower" = lm_sum$coef[2,1] - qt(0.975, df = lm_sum$df[2]) * lm_sum$coef[2, 2],
  "upper" = lm_sum$coef[2,1] + qt(0.975, df = lm_sum$df[2]) * lm_sum$coef[2, 2])
#>     lower     upper 
#> -3.222980 -1.336636

