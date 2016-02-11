# find the median
g <- function(x) { integrate(f, 1, x)$value - 0.5 }
uniroot(g, c(1, 10))$root

