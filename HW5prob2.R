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

plot(q, main="Cloud in a Cell Charge Distribution")

#b)
#using the relaxtion method
target <- 10^(-10)
delta <- 1
h <- 1
n <- 1 #steps to converge
phi <- matrix(rep(0, len=10000), nrow = 100)
phi_prime <- matrix(rep(0, len=10000), nrow = 100)

while (delta > target){
  i = 1
  while (i <= 100){
    j = 1
    while(j <= 100){
      if (i == 1 | i == 100 | j == 1 | j == 100){
        phi_prime[i,j] <- phi[i,j]
      }
      else {
        phi_prime[i,j] <- (phi[i+1,j] + phi[i-1,j] + phi[i,j+1] + phi[i,j-1] + q[i,j])/4
      }
      j = j + 1
    }
    i = i + 1
  }
  delta <- max(abs(phi - phi_prime))
  phi <- phi_prime
  n <- n + 1
}

plot(phi, main="Standard Relaxation Potential Field")
#n = 47058 steps to converge













