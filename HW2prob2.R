# HW2 problem 2, 6.13 from Newman
#Part b)

f <- function(x){
  output <- 5*exp(-x) + x - 5
  return(output)
}

x1 = -1
x2 = 2
m = (x1 + x2)/2
i = 0

while (abs(f(m)) >= 10^(-6)) {
  if (sign(f(x1)) == sign(f(m))){
    x1 = m
    m = (x1 + x2)/2
  }
  if (sign(f(x2)) == sign(f(m))){
    x2 = m
    m = (x1 + x2)/2
  }
  i = i + 1
  print(m)
}





















