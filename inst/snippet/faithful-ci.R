# loglik defined above   # snippet("faithful-mle02", echo = FALSE)
# log-likelihood
loglik.faithful <- function(theta, x) {
  alpha <- theta[1]  
  mu1 <- theta[2]; mu2 <- theta[3]
  sigma1 <- theta[4]; sigma2 <- theta[5]
  
  sum(log(dmix(x, alpha, mu1, mu2, sigma1, sigma2)))
}
m <- mean( ~ duration, data = geyser)
s <-   sd( ~ duration, data = geyser)
ml.faithful <- maxLik(loglik.faithful, x = geyser$duration,
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

