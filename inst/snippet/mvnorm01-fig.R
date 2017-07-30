f <- Vectorize(function(x, y)  mvtnorm::dmvnorm(c(x, y)))
g <- Vectorize(function(x, y)  
  mvtnorm::dmvnorm(
    c(x, y), 
    sigma = rbind(c(1, 0.8), c(0.8, 1))
    ))

