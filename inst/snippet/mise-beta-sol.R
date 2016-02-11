densityplot( ~rbeta(100, .5, .5)|"adjust=1", ylim = c(0, 5), xlim = c(-0.5, 1.5) )
plotFun(dbeta(x, .5, .5) ~ x, add = TRUE, col = "gray50")
densityplot( ~rbeta(100, .5, .5)|"adjust=1/3", adjust = 1/3, ylim = c(0, 5), xlim = c(-0.5, 1.5) )
plotFun(dbeta(x, .5, .5) ~ x, add = TRUE, col = "gray50")
densityplot( ~rbeta(100, .5, .5)|"adjust=3", adjust = 3, ylim = c(0, 5), xlim = c(-0.5, 1.5) )
plotFun(dbeta(x, .5, .5) ~ x, add = TRUE, col = "gray50")
densityplot( ~rbeta(100, .5, .5)|"adjust=0.1", adjust = 0.1, ylim = c(0, 5), xlim = c(-0.5, 1.5) )
plotFun(dbeta(x, .5, .5) ~ x, add = TRUE, col = "gray50")

