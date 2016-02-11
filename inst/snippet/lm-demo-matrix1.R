# create model matrix
intercept <- rep(1,4)
X <- cbind(intercept, x); X
# estimate coeficients
B <- solve(t(X) %*% X) %*% t(X)
B %*% y
# compute fitted values
H <- X %*% B
H %*% y

