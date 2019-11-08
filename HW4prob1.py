#Computational Physics HW 4
#Question 1
#Newman 8.18

from math import sqrt,pi
from numpy import empty,array,arange, copy
from pylab import plot,show, xlabel, ylabel

#Defining constants
t_i = 0
t_f = 20
x_0 = 0
y_0 = 0
delta = 10**(-10)
H = t_f - t_i

#time steps
tpoints = [t_i]
r = array([x_0,y_0],float)
x_points = [r[0]]
y_points = [r[1]]

print("initializing complete")

def f(r):
  print("calling f(r)")
  x = r[0]
  y = r[1]
  f_x = 1-4*x+y*x**2
  f_y = 3*x - y*x**2
  return array([f_x,f_y],float)

def R_n_m(R1, R2, m, n):
  print("calling Rnm")
  print(R1)
  #Computes R_{n,m} , where m <=n
  #:returns R_{n,m} as a vector
  return R2[m - 2] + (R2[m - 2] - R1[m - 2]) / ((n / (n - 1)) ** (2 * (m - 1)) - 1)
    
def mod_midpoint(r, n):
  #Computes the value of r(t+H) given the initial value of r(t) using 
  #modified midpoint method with n steps
  #H interval size
  #returns array with same form as r
  print("calling midpoint")
  r = copy(r)
  h = H / n
  k = r + 0.5 * h * f(r)
  r = r + h * f(k)
    
  for i in range(n - 1):
      k += h * f(r)
      r += h * f(k)
  return 0.5 * (r + k + 0.5 * h * f(r))

def Bulirsch_Stoer_step(r, t, H):
  print("calling BS step")
  
  def compute_row_n(R1, n):
    print("calling compute row")
    if n > 8:
      r1 = Bulirsch_Stoer_step(r, t, H / 2)
      return Bulirsch_Stoer_step(r1, t + H / 2, H / 2)
    else:
      #Compute R_n,1
      R2 = [mod_midpoint(r, n)]
      #Compute the rest of the row
      for m in range(2, n + 1):
        R2.append(R_n_m(R1, R2, m, n))
        #Convert to array to compute error
        R2 = array(R2, float)
        error_vector = (R2[n - 2] - R1[n - 2]) / ((n / (n - 1)) ** (2 * (n - 1)) - 1)
        error = sqrt(error_vector[0] ** 2 + error_vector[1] ** 2)
        # If error is smaller than accuracy, calculation terminates, else repeat with 1 more step
        goal = H * delta
        if error < goal:
          tpoints.append(t + H)
          xpoints.append(R2[n - 1][0])
          ypoints.append(R2[n - 1][1])
          return R2[n - 1]
        else:
          return compute_row_n(R2, n + 1)
  tmp = array([mod_midpoint(r, 1)], float)
  return compute_row_n(tmp, 2)

Bulirsch_Stoer_step(r, t_i, t_f - t_i)

print("preparing to plot")

plot(t_points, x_points, 'b')
# plot(t, y, 'g')
# plot(t, x, 'bo')
# plot(t, y, 'go')
xlabel('t')
ylabel('Concentrations')
show()








