#HW4 problem 2 b)
#clearing console and environment
rm(list = ls())
dev.off()
cat("\014")

library(ggplot2)
library(dplyr)

#Constants
G = 1
M = 1
A = 1
B = 1
h = 10^(-7)

vnorm <- function(vec){
  out <- sqrt(sum(vec^2))
  return(out)
}

#First solving for orbit withouth Dynamical Friction

#This function returns 4 vector with updated v components
f <- function(r, t){
  x_dot <- r[3] 
  y_dot <- r[4]
  vx_dot <- ((-G*M)/(4*(sqrt((r[1])^2 + (r[2])^2)^3)))*r[1] - (A/(B + (sqrt(x_dot^2 + y_dot^2))^3))*x_dot
  vy_dot <- ((-G*M)/(4*(sqrt((r[1])^2 + (r[2])^2)^3)))*r[2] - (A/(B + (sqrt(x_dot^2 + y_dot^2))^3))*y_dot
  out <- c(x_dot, y_dot, vx_dot, vy_dot)
  return(out)
}

RK <- function(r, t, h){
  k1 = h*f(r, t)
  k2 = h*f(r + 0.5*k1, t + 0.5*h)
  k3 = h*f(r + 0.5*k2, t + 0.5*h)
  k4 = h*f(r + k3, t + h)
  out <- r + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
  return(out)  #outputs 4 vector with updated values
}


# Runge-Kutta integrator with adaptive time stepping
t_f <- 20
t <- 0
r <- c(1, 0, 0, 0.4)
delta <- 10^(-5)
t_points <- c(0)
x_points <- c()
y_points <- c()
roe_max = 10^(29)

#Iterative Step
while (t < t_f){
  
  #2 little h steps
  r1 <- RK(r, t, h)
  r1 <- RK(r1, t + h, h)
  
  #one large 2h step
  r2 <- RK(r, t, 2*h)
  #Divide by zero error catch
  if (vnorm(r1 - r2) == 0){
    roe <- roe_max
  } else {
    roe <- (30*h*delta)/(vnorm(r1 - r2))
  }
  
  if (roe >= 1){
    t <- t + 2*h
    r <- r1
    t_points <- c(t_points, t)
    x_points <- c(x_points, r1[1])
    y_points <- c(y_points, r1[2])
    hnew <- h*roe^0.25
    if ((hnew/h) <= 2){
      #Error trapping
      hnew <- 2*h
    }  #Error Trapping
  }   #Jump small enough
  if (roe < 1){
    hnew <- h*roe^0.25
  }  #Jump too large
  h <- hnew
  
  #break when orbit reaches 10^(-7) from peri
  r_b <- c(r[1], r[2])
  if (vnorm(r_b) <= 10^(-7)){
    break
  }
  
}

df <- data.frame(x_points, y_points)
df %>% ggplot(aes(x = x_points, y = y_points)) + geom_point()

r_log <- log10(sqrt(x_points^2 + y_points^2))
df %>% ggplot(aes(x = t_points[-length(t_points)], y = r_log)) + geom_point()

#Printing pericentre value
peri_dist <- sqrt((r1[1])^2 + (r1[2])^2)
peri_dist


