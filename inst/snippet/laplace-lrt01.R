# enter data
x <- c(1.00, -1.43, 0.62, 0.87, -0.66, -0.59, 1.30, -1.23, -1.53, -1.94)
loglik.laplace1 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}
loglik.laplace0 <- function(theta, x) {
    m <- 0; lambda <- theta[1]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}

