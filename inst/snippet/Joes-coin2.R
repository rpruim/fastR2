wilson.ci<- function(x, n, level = 0.95) {
    x = x + 2; n = n + 4
    alpha = 1 - level
    pi.hat <- x / n
    se <- sqrt(pi.hat * (1 - pi.hat) / n)
    z.star <- qnorm(1 - alpha / 2)
    pi.hat + c(-1, 1) * z.star * se
}

