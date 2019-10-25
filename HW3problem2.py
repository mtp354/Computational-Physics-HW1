#HW3 Computational Physics
#Newman 7.9
import math
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg

#a)
#Importing the image
lines = []
with open("blur.txt") as fileobject:
    for line in fileobject:
        lines.append(line)

#seperating each line
for i in range(len(lines)):
    lines[i] = lines[i].split(' ')
    lines[i][1023] = lines[i][1023].rstrip()

#converting to array and plotting
image = np.asarray(lines)
image = np.array(image, dtype=float)
##plt.imshow(image)
##plt.show()

#b)
#constructing the point spread function
def point_spread(x, y):
    out = math.exp(-(x**2 + y**2)/25)
    return out


ps_row = []

#Using symmetry, only looping through non-zero values in upper left corner to save memory
j = 0
ps_list = []

while j < 180:
    ps_row = []
    i = 0
    while i < 180:
        ps_row.append(point_spread(i, j))
        i = i + 1
    ps_list.append(ps_row)
    j = j + 1

print("loop complete")
#Now copying the above code to create each corner
ps_array = np.asarray(ps_list)
ps_array = np.array(ps_array, dtype=float)
ps_array = np.pad(ps_array, ((0, 332),(0, 332)), 'edge')   #filling in top left quadrant with zeroes
ps_array = np.pad(ps_array, ((0, 512),(0, 512)), 'symmetric')   #reflecting in x and y to get point spread array
print(ps_array)
##plt.imshow(ps_array)
##plt.show()

#c)
convulute = np.fft.rfftn(ps_array)
source = np.fft.rfftn(image)
unblur = np.divide(source, convulute, out=np.zeros_like(source), where=convulute>0.001)
unblur = np.fft.irfftn(unblur)
plt.imshow(unblur)
plt.show()












