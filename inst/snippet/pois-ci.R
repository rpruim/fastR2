# loglik.pois defined above  
p <- function(t0) {  
  lrt.stat <- 2 * (loglik.pois(coef(ml.pois10)) - loglik.pois(t0)) 
  1 - pchisq(lrt.stat, df = 1) # p-value        
}
lo <- uniroot(function(t){p(t) - 0.05}, c(0,  coef(ml.pois10))) %>% value()
hi <- uniroot(function(t){p(t) - 0.05}, c(10, coef(ml.pois10))) %>% value()
# confidence interval
c(lo, hi)

