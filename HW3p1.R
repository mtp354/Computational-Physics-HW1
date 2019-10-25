#HW3 Computational Physics
#Newman 7.2,
library(ggplot2)
library(dplyr)
#a)
df <- read.table("C:\\Users\\Matt\\Documents\\Computational Physics\\sunspots.txt")
names(df) <- c("Month", "Sunspots")
ggplot(df, aes(x = df$Month, y = df$Sunspots)) + geom_line()
#Visual estimate thatthe data is periodic repeating every 120 months

#b)
c_k <- c()
k <- seq(1,100,1)
N <- 3143
j <- 0 + 1i

for (kx in k){
  n <- 0
  c_temp <- c()
  for (val in df$Sunspots){
    c_temp <- c(c_temp, val*exp(-2*pi*kx*j*n/N))
    n <- n + 1
  }
  c_temp <- sum(c_temp)
  c_k <- c(c_k, c_temp)
}

c_squared <- (Mod(c_k))^2
df2 <- data.frame(k, c_squared)
ggplot(df2, aes(x = df2$k, y = df2$c_squared)) + geom_line(size = 1)

#c)
which.max(c_squared)
#This gives the 24th element in the vector as the largest, meaning k = 24 is the dominant term
#The period of the sine wave corresponding to k = 24, is 2*pi*24
#Thus the period of the wave is approximately 151 months
#This is similar to my earlier visual estimate of the periodicity







