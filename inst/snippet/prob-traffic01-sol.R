a <- 1
f <- function(x) {1 / x^4}
k <- 1 / integrate(f, a, Inf)$value; k
f <- function(x) {k / x^4}
integrate(f, a, Inf)
integrate(f, 2, 3)

