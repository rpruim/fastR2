free <- 
  maxLik(loglik.laplace1, start = c(m = 0, lambda = 1), x = x); free  
free.est <- coef(free)

