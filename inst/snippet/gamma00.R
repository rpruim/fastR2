require(grid)
inputs <- seq(0.05, 5, by = 0.05)
xyplot(gamma(inputs)~inputs, type = "l",
        ylim = c(0, factorial(4)),
        xlab = "x",
        ylab = expression(Gamma(x)),
        panel = function(x, y, ...){ 
            panel.xyplot(x, y, ...)
            grid.points(1:5, factorial(0:4), gp = gpar(pch = 16, cex = 0.5))
        }
    )

