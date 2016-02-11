# loglik defined above
snippet("loglik-faithful", echo = FALSE)
m <- mean( ~ duration, data = geyser)
s <-   sd( ~ duration, data = geyser)
ml <- maxLik(loglik, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml)
f <- function(a) {
  mla <- maxLik(loglik, x = geyser$duration,
                start = c(a, m - 1, m + 1, s, s), 
                fixed = 1)
  lrt.stat <- 2 * (logLik(ml) - logLik(mla)) 
  pval <- 1 - pchisq(lrt.stat, df = 1)         
  return(pval)
}
lo <- uniroot( function(a){f(a) - 0.05}, c(0.1, mle[1]))$root; lo
hi <- uniroot( function(a){f(a) - 0.05}, c(0.9, mle[1]))$root; hi

