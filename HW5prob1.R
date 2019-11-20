#Homework 5
#Newman 9.7
#clearing console and environment
rm(list = ls())
dev.off()
cat("\014")

#a)
# (x(t+h) - 2*x(t) + x(t-h))/h^2 = -g 
#This is from subsituting in the finite difference approximation
#rearranging to get x(t) on one side
# x(t) = 0.5*[x(t+h) + x(t-h) +g*h^2]

f <- function(t){
  out <- 0.5*(f(t+h) + f(t-h) + 9.81*h^2)
}

#b)
target <- 10^(-6)
h <- 0.1
delta = 1.0
t <- seq(0, 10, h)
x <- seq(0, 0, length.out = 101)
x_prime <- seq(0, 0, length.out = 101)

while (delta > target){
  i = 1
while (i <= length(t)){
  if (t[i] == 0 || t[i] == 10){
    x_prime[i] <- x[i]
  }
  else {
    x_prime[i] <- 0.5*(x[i+1] + x[i-1] + 9.81*h^2)
  }
  i = i + 1
}
delta <- max(abs(x - x_prime))
x <- x_prime
}








