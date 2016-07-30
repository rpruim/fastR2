x <- seq(0, 1, by = 0.01)
xyplot( x * (1-x) ~ x, 
    lwd = 2, type = "l",
    main = "Variance of a Bernoulli random variable",
    xlab = expression(pi),
    ylab = expression(Var(X))
    )

