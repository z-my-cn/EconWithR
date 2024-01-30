# Econometrics with R
# Chapter 01 Introduction ------------------------------------------------------


# 1.1 Colophon -----------------------------------------------------------------


# 1.2 A Very Short Introduction to R and RStudio -------------------------------

1 + 1
#> [1] 2


y <- c(1, 2, 3, 4, 5)
y
#> [1] 1 2 3 4 5


hello <- c("Hello", "World")
hello
#> [1] "Hello" "World"


# generate the vector `z`
z <- seq(from = 1, to = 5, by = 1)

# compute the mean of the entries in `z`
mean(z)
#> [1] 3