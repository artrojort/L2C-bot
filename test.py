#Cubic Bezier curves
#Arturo Rojas Ortiz
#A01039185
#Computer Graphics Lu-Ju 8:30
#October 14, 2019

import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d, Axes3D #<-- Note the capitalization! 
import numpy as np

# Data for plotting
def beziercuadratic():
    points = 100
    t = np.linspace(0, 1, points)  

    #Example 1
    #ptX = [1, 5, 4, 2]
    #ptY = [1, 0, 6, 5]

    #Example2
    #ptX = [1, 2, 4, 5]
    #ptY = [1, 5, 6, 0]

    #Example3
    ptX = [1, 4, 2, 5]
    ptY = [1, 6, 5, 0]

    M = [
            [-1, 3, -3, 1],
            [ 3, -6,3, 0],
            [-3, 3, 0, 0],
            [1, 0, 0, 0],
        ]

    temp3 = []
    temp2 = []
    temp1 = []
    temp0 = []

    for i in t:
        temp3.append(i**3)
        temp2.append(i**2)
        temp1.append(i)
        temp0.append(1)

    T = []
    T.append(temp3)
    T.append(temp2)
    T.append(temp1)
    T.append(temp0)

    Cx = np.dot(np.dot(ptX, M), T)
    Cy = np.dot(np.dot(ptY, M), T)

    # Formula
    # x = (-4**3 + 3 * t**2 - 3 * t + 1) * pt0[0] + (3 * t**3 - 6 * t**2 + 3 * t) * pt1[0] + (-3 * t**3 + 3 * t**2) * pt2[0] + t**3 * pt3[0]
    # y = (-4**3 + 3 * t**2 - 3 * t + 1) * pt0[1] + (3 * t**3 - 6 * t**2 + 3 * t) * pt1[1] + (-3 * t**3 + 3 * t**2) * pt2[1] + t**3 * pt3[1]

    fig, ax = plt.subplots()
    ax.plot(ptX, ptY)
    ax.plot(Cx, Cy)
    plt.title("Cubic Bezier")
    plt.show()


beziercuadratic()
