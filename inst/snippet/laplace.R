dlaplace <- function(x, theta, lambda) {
    0.5 * lambda * exp(-lambda * abs(x-theta))
}
# two ways to do plaplace:
integrate(function(x) {dlaplace(x, 1, 2)}, -Inf, Inf)     # should = 1
plaplace1 <- function(q, theta = 0, lambda = 1) {
    integrate(function(x) {dlaplace(x, theta, lambda)}, -Inf, q)$value
}

plaplace2 <- function(q, theta = 0, lambda = 1) {
    if (q < theta) return(0.5 * (1-pexp(theta-q, lambda)))
    return(0.5 + 0.5 * pexp(q-theta, lambda))
    }
# should get same results either way:
plaplace1(3, lambda = 2, theta = 1)
plaplace1(3, lambda = 2, theta = 1) - plaplace1(-3, lambda = 2, theta = 1)
plaplace2(3, lambda = 2, theta = 1)
plaplace2(3, lambda = 2, theta = 1) - plaplace2(-3, lambda = 2, theta = 1)

