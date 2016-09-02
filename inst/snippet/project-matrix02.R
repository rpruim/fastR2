t(A) %*% v1 
t(A) %*% v2
x <- 1:4
coefs <- t(A) %*% x; coefs
pr <- A %*% coefs; pr
remainder <- x - pr; remainder
dot(remainder, v1)    # should be 0
dot(remainder, v2)    # should be 0


