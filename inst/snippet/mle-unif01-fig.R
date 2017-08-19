L <- function(theta) { (1 / theta)^6 * (theta >= 8.7) }
plotFun(L(theta) ~ theta, theta.lim = c(6, 15),
        xlab = expression(theta), ylab = expression(L(theta)))

