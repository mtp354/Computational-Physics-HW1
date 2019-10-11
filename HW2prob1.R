# HW2 problem 1, 6.11 from Newman
library(ggplot2)
# Begin with question 6.10
x_0 = 0.5 #tested using multiple values, must be greater than zero to converge
x = c(x_0)
i = 1

#6.10 a)
c = 2
while (i < 50){
  x = 1 - exp(-c*x)
  print(x)
  i = i + 1
}

#6.10 b)
X = c() #empty vector to store the converged value for each c value
x = c(x_0)
c = seq(0.01, 3, 0.01)
for (val in c){
  while (i < 10){
    x = c(x, 1 - exp(-val*x))
    i = i + 1
  }
  X = c(X, x[49])
  x = c(x_0)
  i = 1
}
plot(x = c, y = X)

#6.11 b)
x_0 = 0.5
x = x_0
X = c(x_0, 0.6)
i = 2
c = 2

while (abs(X[i] - X[i-1]) >= 10^(-6)){
  x <- 1 - exp(-c*x)
  X <- c(X, x)
  i = i + 1
  print(i)
  print(X)
}
print(i - 2)

#6.11 c)
w = 0.70
x_0 = 0.5
x = x_0
X = c(x_0, 0.6)
i = 2
c = 2

while (abs(X[i] - X[i-1]) >= 10^(-6)){
  x <- (1 + w)*(1 - exp(-c*x)) - w*x
  X <- c(X, x)
  i = i + 1
  print(i)
  print(X)
}
print(i - 2)

#6.11 d)
# a negative value of w is useful for a function if you're starting point is to the right of the root








