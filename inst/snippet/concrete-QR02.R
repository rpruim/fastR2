x1 <- Concretemod$limestone; x2 <- Concretemod$water
X <- cbind(1, x1, x2)
Q <- cbind(
  1 / sqrt(nrow(Concretemod)), 
  v1 / vlength(v1), 
  w2 / vlength(w2)) 
t(Q) %*% Q %>% round(4)           # should be the identity matrix

