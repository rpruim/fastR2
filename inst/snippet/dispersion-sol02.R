T <- function(x) c(T = var(x) / mean(x))
# one-sided p-values
Sims1 <- (do(10000) * T(rpois(15, lambda = 1))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims5 <- (do(10000) * T(rpois(15, lambda = 5))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims50 <- (do(10000) * T(rpois(15, lambda = 50))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
# It isn't necessary to mulitiply T by (n-1) to assess linearity in a qq-plot
xqqmath(~ T, data = Sims1, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 1")
xqqmath(~ T, data = Sims5, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 5")
xqqmath(~ T, data = Sims5, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 50")
# now we compare p-values to uniform distribution, zooming in on small p-values
xqqmath( ~ p.val, data = Sims1, distribution = qunif, type = "l", main = "lambda = 1", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))
xqqmath( ~ p.val, data = Sims5, distribution = qunif, type = "l", main = "lambda = 5", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))
xqqmath( ~ p.val, data = Sims50, distribution = qunif, type = "l", main = "lambda = 50", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))


