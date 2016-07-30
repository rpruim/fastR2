X <- cbind(1, x1, x2); X
Q <- cbind(
  1/sqrt(nrow(concretemod)), 
  v1 / vlength(v1), 
  w2 / vlength(w2)) 
Q %>% round(4)
t(Q) %*% Q %>% round(4)           # should be the identity matrix
R <- t(Q) %*% X; R %>% round(4)   # should be upper triangular
Q %*% R %>% round(4)              # should be X

