xyplot(dbinom(0:20, 20, 0.1) ~ 0:20, type = "h", xlim = c(0,10),
            lwd = 2,
            xlab = expression(x),
            main = "Binomial vs Normal(n=20, pi=0.10)",
            ylab = "density",
            panel = function(x, y, ...){
                panel.xyplot(x, y, col = "black", ...);
                panel.mathdensity(dnorm,
                                  list(mean = 2, sd = sqrt(0.1 * 0.9 * 20)), ...);
            })

