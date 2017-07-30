f <- function(n) { qt(0.975, n-1) * 2 / sqrt(n) - 0.25}
uniroot(f, c(10, 1000))

