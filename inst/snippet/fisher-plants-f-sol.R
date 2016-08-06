fisher.pval <- 
  Vectorize(
    vectorize.args = "theta0",
    function(theta0, x) {
      w <- 2 * (loglik.fisher(theta.hat, x) - loglik.fisher(theta0, x))
      1 - pchisq(w, df = 1)
    }
  )
lo <- 
  uniroot(
    function(t0) fisher.pval(t0, fisher.counts) - 0.05, c(0, theta.hat)) %>% 
  value()
hi <- 
  uniroot(
    function(t0) fisher.pval(t0, fisher.counts) - 0.05, c(1, theta.hat)) %>% 
  value()
c(lo, hi)

