# find the median
g <- function(x) { integrate(f, a, x)$value - 0.5 }
uniroot(g, c(1, 10))$root

