loglik.normal <- function(theta, x) {
  mu <- theta[1]; sigma <- theta[2]
  if (sigma < 0) return(NA)     # alert maxLik() to invalid values of sigma
  dnorm(x, mu, sigma, log = TRUE)
}

