loglik2 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return( sum( log( dlaplace( x, m, lambda) ) ) )
}
mle2 <- maxLik(loglik2, start = c(0,1), x = x)
summary(mle2)

