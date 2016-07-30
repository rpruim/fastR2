score.ci <- function(x, n, level = 0.95) {
    alpha = 1 - level
    z.star <- qnorm(1 - alpha / 2)
    pi.hat <- x / n
    A <- pi.hat + z.star^2 / (2 * n)
    B <- z.star * sqrt((pi.hat * (1 - pi.hat) / n) 
	                  + (z.star^2 / (4 * n^2)))
    D <- 1 + z.star^2 / n
    # interval is (A +- B) / D
    (A + c(-1, 1) * B) / D
}

