# log likelihood function
loglik <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    sum(dbeta(x, theta[1], theta[2], log = TRUE)) 
}
# alternative way to define the log-likelihood
loglik2 <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    dbeta(x, theta[1], theta[2], log = TRUE) 
}

