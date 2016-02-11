f <- function(x) {1/x^4}
k <- 1 / integrate(f, 1, Inf)$value; k
f <- function(x) {k / x^4}
integrate(f, 1, Inf)
integrate(f, 2, 3)
xf <- function(x) { x * f(x) }

