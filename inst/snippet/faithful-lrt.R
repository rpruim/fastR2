# seed the algorithm  
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml <- maxLik(loglik.faithful, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml); mle
loglik.faithful(mle, x = geyser$duration)
logLik(ml)        # makLik can caclulate this log-likelihood for us

ml0 <- maxLik(loglik0.faithful, x = geyser$duration,
              start = c(m - 1, m + 1, s, s))
mle0 <- coef(ml0); mle0
logLik(ml0)                      
lrt.stat <- 2 * (logLik(ml) - logLik(ml0)); lrt.stat
1 - pchisq(lrt.stat, df = 1)     # p-value based on asymptotic distribution

