#Homework 5
#clearing console and environment
library(plot.matrix)
rm(list = ls())
dev.off()
cat("\014")

particles <- read.table("/home/data1/mtp354/Downloads/particles.txt")
names(particles) <- c("x","y")


q <- matrix(rep(0, len=10000), nrow = 100)

#populating grid using cloud in a cell
particles <- round(particles, 0)

a = 1
while (a <= 24478){
  q[particles$x[a], particles$y[a]] <- q[particles$x[a], particles$y[a]] + 1
  a = a + 1
}

#c)
#Gauss-Seidel Overrelaxation method

target <- 10^(-10)
delta <- 1
h <- 1
n <- 1 #steps to converge
phi <- matrix(rep(0, len=10000), nrow = 100)
phi_old <- matrix(rep(0, len=10000), nrow = 100)

z <- (1+sqrt(5))/2




loop <- function(w){
while (delta > target){
  i = 1
  while (i <= 100){
    j = 1
    while(j <= 100){
      if (i == 1 | i == 100 | j == 1 | j == 100){
        phi[i,j] <- 0
      }
      else {
        phi[i,j] <- (phi[i+1,j] + phi[i-1,j] + phi[i,j+1] + phi[i,j-1] + q[i,j])*(1+w)/4 - w*phi[i,j]
      }
      j = j + 1
    }
    i = i + 1
  }
  delta <- max(abs(phi - phi_old))
  phi_old <- phi
  n <- n + 1
}
  print(n)
  return(n)
}
# plot(phi, main="Gauss-Seidel Over-Relaxation Potential Field")

w_1 <- 0.01
w_4 <- 0.99
w_3 <- (w_4 - w_1)/z + w_1
w_2 <- w_4 - w_3 + w_1

eps <- 1
w_list <- c()

while (eps > 10^(-3)){
if(loop(w_2) < loop(w_3)){
  w_4 <- w_3
  w_3 <- w_2
  w_2 <- w_4 - w_3 + w_1
}  else {
  w_1 <- w_2
  w_2 <- w_3
  w_3 <- w_4 - w_2 + w_1
}
eps <- abs(w_3 - w_2)
# print(eps)
w_list <- c(w_list, (w_2 + w_3)/2)
}


plot(w_list)










