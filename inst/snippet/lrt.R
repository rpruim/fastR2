free <- maxLik(loglik1, start = c(0,1), x = x)
summary(free)
free.est <- coef(free)
null <- maxLik(loglik0, start = c(1), x = x)
null
null.est <- coef(null)
stat <- 2 * (loglik1(free.est, x) - loglik0(null.est, x)); stat
1 - pchisq(stat, df = 1)          # p-value based on asymptotic distribution

