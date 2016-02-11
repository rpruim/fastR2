llik <- function (p, t = 14) {
    14*log(p) + 26 * log(1-p)
}
xpts <- seq(0, 1, by = 0.005)
xyplot( llik(xpts) ~ xpts,
			type = 'l',
            lwd = 2, 
            xlab = expression(pi),
            ylab = "log-likelihood"
			)

