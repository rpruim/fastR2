free <- maxLik(loglik.laplace1, start = c(m = 0, lambda = 1), x = x); free  
free.est <- coef(free)
null <- maxLik(loglik.laplace0, start = c(lambda = 1), x = x); null
null.est <- coef(null)
w <- 2 * (loglik.laplace1(free.est, x) - loglik.laplace0(null.est, x)); w
1 - pchisq(w, df = 1)          # p-value based on asymptotic distribution

