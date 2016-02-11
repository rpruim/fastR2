x <- c(1.6, 2.8, 6.2, 8.2, 8.7)
loglik.unif <- function(theta, x) {
  res <- sum ( dunif(x, 0, theta, log = TRUE) )
  ifelse( is.finite(res), res, NA)
}

lik.unif <- function(theta, x) {
  res <- prod ( dunif(x, 0, theta, log = FALSE) )
  ifelse( is.finite(res), res, NA)
}

