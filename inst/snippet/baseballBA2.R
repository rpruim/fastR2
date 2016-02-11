# log likelihood function
loglik <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    sum(dbeta(x, theta[1], theta[2], log = TRUE)) 
}

