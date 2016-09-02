plotFun(ilogit(3 + 2 * x) ~ x, x.lim = c(-6, 3), lwd = 3, col = "gray70")
plotFun(pnorm(1.5 * b1 + b1 * x) ~ x, b1 = sqrt(2 * pi)/2, 
        add = TRUE, col = "red")

