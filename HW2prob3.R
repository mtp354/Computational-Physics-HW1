#Problem 3 HW2
library(dplyr)
library(ggplot2)


#Implementation of Gradient descent method on simple function
a = c(1, 20)
b = c(0, 1)
i = 2
gamma = 0.5

test_function <- function(x, y){
  output <- (x - 2)^2 + (y - 2)^2
  return(output)}

while (i < 100) {
    a <- c(a, a[i] - gamma*(test_function(a[i], b[i-1]) - test_function(a[i-1], b[i-1]))/(a[i] - a[i-1]))
    b <- c(b, b[i] - gamma*(test_function(a[i-1], b[i]) - test_function(a[i-1], b[i-1]))/(b[i] - b[i-1]))
    i = i + 1
}
# converges to (2, 2) as is expected, approximately 30 iterations for 4 dp precision
# applying our methods to the provided COSMOS data
rm(list=ls())
cat("\014")

df <- read.table("/home/data1/mtp354/Comp-Phys-HW2/smf_cosmos.dat",
                 sep = " ",header = F, na.strings ="", stringsAsFactors= F)
names(df) <- c("log M_gal", "n(M_gal)", "error in n(M_gal)")

ggplot(data = df, aes(x = df$`log M_gal`, y = log(df$`n(M_gal)`))) + geom_point() + geom_errorbar(data = df, 
                aes(x = df$`log M_gal`, ymin = log(df$`n(M_gal)` - 0.5*df$`error in n(M_gal)`),  
                    ymax = log(df$`n(M_gal)` + 0.5*df$`error in n(M_gal)`)))


#Defining our Chi Squared and Schechter Functions
schechter <- function(input, M_gal){ #input = c(phi_star, M_star, alpha)
  phi_star <- 10^(input[1])       #stepping in log phi_star
  M_star <- 10^(input[2])            #stepping in log M_gal
  alpha <- input[3]
  M_gal <- 10^(M_gal)
  n <- phi_star*(M_gal/M_star)^(alpha + 1)*exp(-M_gal/M_star)*log(10)
  return(n)
}

# y1 <- schechter(c(-3.2, 11.5, -0.5), df$`log M_gal`)           #testing values
# plot(x = df$`log M_gal`, y = log(y1))
cat("\014")

chi_2 <- function(input, M_gal, y, sigma){
  f <- schechter(input, M_gal)
  output <- sum(((f - y)^2)/sigma^2)
  return(output)
}

# chi_2(c(-3.2, 11.5, -0.5), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`) #testing values

#making a 3 dimensional gradient descent function
grad <- function(initial, gamma, delta){
  a <- c(initial[1] - delta, initial[1]) #generating the initial values
  b <- c(initial[2] - delta, initial[2])
  c <- c(initial[3] - delta, initial[3])
  #generating the next iterations
  i = 2
  while (i < 1000){
  a <- c(a, a[i] - gamma*(chi_2(c(a[i], b[i-1], c[i-1]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`) -
        chi_2(c(a[i-1], b[i-1], c[i-1]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`))/(a[i] - a[i-1]))
  
  b <- c(b, b[i] - gamma*(chi_2(c(a[i-1], b[i], c[i-1]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`) -
        chi_2(c(a[i-1], b[i-1], c[i-1]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`))/(b[i] - b[i-1]))
  
  c <- c(c, c[i] - gamma*(chi_2(c(a[i-1], b[i-1], c[i]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`) -
        chi_2(c(a[i-1], b[i-1], c[i-1]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`))/(c[i] - c[i-1]))
  i = i + 1
  }
  output <- list(a, b, c)
  return(output)
}


#Now applying gradient descent to my chi_2 function
g = 0.0001 #gamma
d = 0.0001 #delta
out <- grad(c(-3.2, 11.5, -0.5), g, d)
rm(g, d)

# plot(x = seq(1, 1000, 1), y = out[[3]])
# 
# cat("\014")
# inp <- c(out[[1]][1000], out[[2]][1000], out[[3]][1000])
# out <- schechter(inp, df$`log M_gal`)
# df <- df %>% mutate(output = out)
# 
# 
# plot(x = df$`log M_gal`, log(out))
# ggplot(data = df, aes(x = df$`log M_gal`, y = log(df$`n(M_gal)`))) + geom_point() + geom_errorbar(data = df,
#           aes(x = df$`log M_gal`, ymin = log(df$`n(M_gal)` - 0.5*df$`error in n(M_gal)`),
#                     ymax = log(df$`n(M_gal)` + 0.5*df$`error in n(M_gal)`))) +geom_line(
#                       data = df, aes(x = df$`log M_gal`, y = log(df$output), colour = "red"))
# 
# chi_2(inp, df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`)

i = 1
c <- c()
while (i < 1000){
  c <- c(c, chi_2(c(out[[1]][i], out[[2]][i], out[[3]][i]), df$`log M_gal`, df$`n(M_gal)`, df$`error in n(M_gal)`))
  i = i + 1 
}

i <-  seq(1, 200, 1)
plot(x = i, y = c[1:200])







