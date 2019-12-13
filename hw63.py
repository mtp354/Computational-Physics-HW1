######################    Computational Physics Homework 6 Problem 3    ##########################################
# Power Spectra of random numbers and random walks
from math import log,cos,sin,pi,sqrt
import matplotlib.pyplot as plt
import random
import numpy as np


def LCG(n, seed, multiplier, increment, modulus):
    rand_list = []
    X_n = seed
    for k in range(n):
        X_n = (multiplier*X_n + increment) % modulus
        rand_list.append(X_n/modulus)
    return rand_list[random.randint(1,9999)]

rand_list = LCG(10000, 123412, 1103515245, 12345, 2**31)   #from glibc on Wiki

#generating gaussian random values
sigma = 1
def gaussian():
    r = sqrt(-2*(sigma**2)*log(1-LCG(10000, 123412, 1103515245, 12345, 2**31)))
    theta = 2*pi*LCG(10000, 123412, 1103515245, 12345, 2**31)
    x = r*cos(theta)
    y = r*sin(theta)
    return x,y

x_values=[]
y_values=[]
for i in range(10000):
    x,y=gaussian()
    x_values.append(x)
    y_values.append(y)
    checkmarks = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000]
    if i in checkmarks:
        print(i)



##plt.hist(x_values, 100)
##plt.yscale('log')
##plt.show()

#Generating power spectrum
def DFT(list_input):
    N = len(list_input)
    C = np.zeros(N//2+1,complex)   
    for k in range(N//2+1):
        for n in range(N):
            C[k]+=list_input[n]*np.exp(-2j*np.pi*k*n/N)
    return C

##m = DFT(x_values)
##C_squared = abs(m)**2
##plt.figure(dpi=150)
##plt.loglog(C_squared)
##plt.ylabel('$|c_k|$ $^{2}$')
##plt.xlabel('k')
##plt.grid()
##plt.show()

####################    Random Walk    ######################################
  
# step total number
n = 10000
  
#creating empty arrays for holding values
u = x_values 
v = y_values

# filling the coordinates with random variables 
for i in range(1, n): 
    val = random.randint(1, 4) 
    if val == 1: 
        u[i] = u[i - 1] + 1
        v[i] = v[i - 1] 
    elif val == 2: 
        u[i] = u[i - 1] - 1
        v[i] = v[i - 1] 
    elif val == 3: 
        u[i] = u[i - 1] 
        v[i] = v[i - 1] + 1
    else: 
        u[i] = u[i - 1] 
        v[i] = v[i - 1] - 1
      

plt.title("Random Walk ($n = " + str(n) + "$ steps)") 
plt.plot(u) 
plt.show() 

m = DFT(u)
C_squared = abs(m)**2
plt.figure(dpi=150)
plt.loglog(C_squared)
plt.ylabel('$|c_k|$ $^{2}$')
plt.xlabel('k')
plt.grid()
plt.show()







