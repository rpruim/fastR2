B <- A[c(1, 3, 2), c(1, 3, 2)]; B
# This is NOT the covariance matrix
B[-3, -3] %*% t(B[-3, -3])
# Nor is this (since it is the same as the matrix above)
A[-2, -2] %*% t(A[-2, -2])

