######################    Computational Physics Homework 6 Problem 1    ##########################################
# Newman 10.9
import numpy as np
import random
import matplotlib
import matplotlib.pyplot as plt
import math

#generating starting array
grid = np.zeros((20,20), dtype=int)
i = 0
while i < 20:
    j = 0
    while j < 20:
        if random.randint(1,2) == 1:
            grid[i][j] = -1
        else:
            grid[i][j] = 1
        j = j + 1
    i = i + 1


##plt.imshow(grid, cmap='hot', interpolation='nearest')
##plt.show()

#defining total energy function
def get_energy(array):
    energy = 0
    i = 0
    while i < 19:
        j = 0
        while j < 19:
            energy = energy - array[i+1][j]*array[i][j+1]
            j = j + 1
        i = i + 1
    return energy

#applying the metropolis algorithm
k = 0
M = []
while k < 10000:
    e_old = get_energy(grid)
    a = random.randint(0,19)
    b = random.randint(0,19)
    grid[a][b] = -grid[a][b]  #flipping element
    e_new = get_energy(grid)
    if e_new > e_old:
        p = math.exp(-(e_new - e_old))
        if random.random() > p:
            grid[a][b] = -grid[a][b]   #flip element back
    M.append(np.sum(grid))
    k = k + 1

print("All done")
plt.plot(M)
plt.show()
##plt.imshow(grid, cmap='hot', interpolation='nearest')
##plt.show()































