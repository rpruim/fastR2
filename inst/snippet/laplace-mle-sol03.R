loglik.laplace2 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(dlaplace(x, m, lambda))))
}
ml.laplace2 <- maxLik(loglik.laplace2, start = c(0, 1), x = x)
ml.laplace2

