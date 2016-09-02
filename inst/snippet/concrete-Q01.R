Q <- 
  cbind( 
    w1 / vlength(w1)^2, 
    w2 / vlength(w2)^2)
t(Q) %*% y

