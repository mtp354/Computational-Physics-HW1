#Problem 3
library(dplyr)
library(ggplot2)
library(tidyr)

#Importing Data
df <- read.table("C:\\Users\\Matt\\Documents\\Computational Physics\\lcdm_z0.matter_pk",
                 sep = " ",header = F, nrows = 500, na.strings ="", stringsAsFactors= F)

#Formating Data Frame Removing Unneeded Columns
names(df) <- c("k", "P(k)", "V3", "V4")
df <- df %>% select("k", "P(k)")

ggplot(data = df, aes(x = log(df$k),y=log(df$`P(k)`))) + geom_point()

#Interpolation
new_dat_a <- as.data.frame(spline(x = df$k, y = df$`P(k)`, n = 1000, method = "natural",
                  xmin = min(df$k), xmax = max(df$k[250])))
new_dat_b <- as.data.frame(spline(x = df$k, y = df$`P(k)`, n = 10000, method = "natural",
                                  xmin = min(df$k), xmax = max(df$k[500])))
new_dat <- data.frame(x = c(new_dat_a$x, new_dat_b$x), y = c(new_dat_a$y, new_dat_b$y))

# Visualization of interpolation
ggplot(data = new_dat, aes(x = log(new_dat$x), y = log(new_dat$y))) + geom_point()

r <- seq(50, 120, length.out = 70)


#using the trapezoid rule to integrate the kernal
n <- 11000
a <- 0
b <- max(new_dat$x)
h <- (b-a)/n

correlation <- c()
for (val in r){
  kernal <- (1/(2*pi/2))*((new_dat$x)^2)*(new_dat$y*(sin(val*new_dat$x))/(val*new_dat$x))
  integral <- h*(kernal[1] + kernal[11000] + sum(kernal[2:10999]))
  correlation <- c(correlation, integral)
}

plot(x = r, y = correlation)

#Now making the r^2 multiple correlation plot
corr_r2 <- c()
for (val in r){
  kernal <- (1/(2*pi/2))*((new_dat$x)^2)*(new_dat$y*(sin(val*new_dat$x))/(val*new_dat$x))
  integral <- h*(kernal[1] + kernal[11000] + sum(kernal[2:10999]))
  corr_r2 <- c(corr_r2, integral*val^2)
}

plot(x = r, y = corr_r2)


