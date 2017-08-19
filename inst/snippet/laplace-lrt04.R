w <- 
  2 * (loglik.laplace1(free.est, x) - loglik.laplace0(null.est, x)) 
w
1 - pchisq(w, df = 1)          # p-value based on asymptotic distribution

