# Computational Physics HW 1
library(float)
library(ggplot2)
library(dplyr)

#Problem 1
#a)
x <- fl(0.1)
h <- seq(-1, -7, length.out = 30)
h <- 10^h %>% fl()

# Derivative of cos(x) using Forward, Central, and Extrapolated Difference methods
cos_f <- (cos(x+h)-cos(x))/h
cos_c <- (cos(x+h)-cos(x-h))/(2*h)
cos_e <- (-cos(x+2*h)+8*cos(x+h)-8*cos(x-h)+cos(x-2*h))/(12*h)

# Derivative of exp(x) using Forward, Central, and Extrapolated Difference methods
exp_f <- (exp(x+h)-exp(x))/h
exp_c <- (exp(x+h)-exp(x-h))/(2*h)
exp_e <- (-exp(x+2*h)+8*exp(x+h)-8*exp(x-h)+exp(x-2*h))/(12*h)

#b)
#Relative Error
error_cos_f <- abs((cos_f + sin(x))/(sin(x))) %>% fl()
error_cos_c <- abs((cos_c + sin(x))/(sin(x))) %>% fl()
error_cos_e <- abs((cos_e + sin(x))/(sin(x))) %>% fl()

error_exp_f <- abs((exp_f - exp(x))/(exp(x))) %>% fl()
error_exp_c <- abs((exp_c - exp(x))/(exp(x))) %>% fl()
error_exp_e <- abs((exp_e - exp(x))/(exp(x))) %>% fl()


plot(x = log(h), y = log(error_cos_f), col = "blue", type = "l", main = 
       "Relative Error for Cosine Derivative Estimates")
lines(x = log(h), y = log(error_cos_c), col = "red")
lines(x = log(h), y = log(error_cos_e), col = "green")

plot(x = log(h), y = log(error_exp_f), col = "blue", type = "l", main = 
       "Relative Error for Exp Derivative Estimates")
lines(x = log(h), y = log(error_exp_c), col = "red")
lines(x = log(h), y = log(error_exp_e), col = "green")








