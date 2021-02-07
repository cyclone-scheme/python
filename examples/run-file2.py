import numpy as np

n = 8

# Create a nxn matrix filled with 0
matrix = np.zeros((n, n), dtype=int)

# fill 1 with alternate rows and column
matrix[::2, 1::2] = 1
matrix[1::2, ::2] = 1

# Print the checkerboard pattern
for i in range(n):
    for j in range(n):
        print(matrix[i][j], end=" ")
    print()
