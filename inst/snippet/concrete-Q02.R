x1 <- Concretemod$limestone; x2 <- Concretemod$water
Q <- 
  cbind( 
    1 / nrow(Concretemod),
    w1 / vlength(w1)^2, 
    w2 / vlength(w2)^2)
alpha <- t(Q) %*% y; alpha
beta0 <- alpha[1] - alpha[2] * mean(x1) - alpha[3] * mean(x2)
beta0

