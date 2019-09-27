# Computational Physics HW 1
library(float)
library(ggplot2)
library(dplyr)

#Problem 2
# a)
a <- 0 
b <- 1 

#True area under curve
Area_true <- (-exp(-1)+exp(0)) %>% fl()

#Relative Error
N <- seq(100, 100000, length.out = 30)
error_m <- c()
error_t <- c()
error_s <- c()

#midpoint method
for (val in N)
{
  h <- (b-a)/val %>% fl()
  x <- seq(a, b, length.out = val)
  A_m <- (sum(exp(-(x[-length(x)]+h/2))*h)) 
  error_m <- c(error_m, (abs(A_m - Area_true)/Area_true)) %>% fl()
}

#trapezoid rule
for (val in N)
{
  h <- (b-a)/val %>% fl()
  x <- seq(a, b, length.out = val) 
  A_t <- (h*(0.5*(exp(-a)+exp(-b)) + sum(exp(-(x[-length(x)])))))
  error_t <- c(error_t, (abs(A_t - Area_true)/Area_true)) %>% fl()
  }

#simpsons rule
for (val in N)
{
  h <- (b-a)/val %>% fl()
  x <- seq(a, b, length.out = val)
  x_odd <- x[c(T, F)]
  x_even <- x[c(F, T)]
  A_s <- (h*((1/3)*((exp(-a)+exp(-b))) + (4/3)*sum(exp(-x_odd)) + (2/3)*sum(exp(-x_even))))
  error_s <- c(error_s, (abs(A_s - Area_true)/Area_true)) %>% fl()
  }

# b) Plotting Log Error vs Log N
plot(x = log(N), y = log(error_t), type = "l", col = "red")
lines(x = log(N), y = log(error_m), col = "blue")
lines(x = log(N), y = log(error_s), col = "green")








