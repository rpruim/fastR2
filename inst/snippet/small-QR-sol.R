X <- cbind(1, x1, x2)
Q <- cbind(
  v0 / vlength(v0),
  v1 / vlength(v1),
  w2 / vlength(w2)
)

# orthogonality check for Q
t(Q) %*% Q %>% round(3)

# solve QR = X for R and check that it is upper diagonal
R <- t(Q) %*% X; R %>% round(3)  

# check that X = QR (up to round off)
range(X - Q %*% R)          

# find coefficients
solve(R) %*% t(Q) %*% y

# check that this matches coefficients from lm()
range( solve(R) %*% t(Q) %*% y - coef(model) )

