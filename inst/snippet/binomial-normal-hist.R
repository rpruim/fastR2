p <- ppoints(50);
n <- rep(c(10, 40, 80, 800), each = length(p))
p <- rep(p, times = 4)

pi <- rep(c(0.5, 0.3, 0.1, 0.05), each = length(p))
p <- rep(p, times = 4)
n <- rep(n, times = 4)

p <- xyplot(
        qbinom(p, n, pi) ~ qnorm(p, n * pi, sqrt(n * pi * (1-pi))) |
        paste("n=", n, sep = "") * paste("pi=", pi, sep = ""),
        scales = list(relation = "free"),
        cex = 0.6,
    ylab = expression(qbinom(p, n, pi)),
    xlab = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi)))),
    panel = function(x, y, ...){
        panel.abline(0, 1, ...);
        panel.xyplot(x, y, ...);
    })
latticeExtra::useOuterStrips(p)

