x <- c(1.6, 2.8, 6.2, 8.2, 8.7)
loglik <- function(theta, x) {
     sum ( dunif(x, 0, theta, log = TRUE) )
}
lik <- function(theta, x) {
     prod ( dunif(x, 0, theta, log = FALSE) )
}

