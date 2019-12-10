######################    Computational Physics Homework 6 Problem 2    ##########################################
# Newman 10.11
import numpy as np
import random
import matplotlib
import matplotlib.pyplot as plt
import math


#generating starting array
grid = np.zeros((50,50), dtype=int)

#defining total energy function
def get_energy(array):
    energy = - np.sum(grid)
    return energy
T = 1
k = 0
E = []

while k < 10000:
    a1 = random.randint(1,48)
    b1 = random.randint(1,48)
    a2 = a1
    b2 = b1
    c = random.randint(1,4)
    if c == 1:
        a2 = a1 - 1
    elif c == 2:
        a2 = a1 + 1
    elif c == 3:
        b1 = b1 - 1
    elif c == 4:
        b2 = b1 + 1
    #now that a random pair of adjacent points have been chosen
    p = math.exp(-1/T)
    if grid[a1][b1] == 0:
        if grid[a2][b2] == 0:
            grid[a1][b1] = grid[a1][b1] + 1
            grid[a2][b2] = grid[a2][b2] + 1
    elif grid[a1][b1] == 1:
        if grid[a2][b2] == 1:
            if random.random() < p:
                grid[a1][b1] = grid[a1][b1] - 1
                grid[a2][b2] = grid[a2][b2] - 1
    E.append(get_energy(grid))
    k = k + 1

plt.imshow(grid, cmap='hot', interpolation='nearest')
plt.show()

plt.plot(E)
plt.show()



















