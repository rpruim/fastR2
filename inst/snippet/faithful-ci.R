# loglik defined above   
snippet("faithful-mle01", echo = FALSE)
snippet("faithful-mle02", echo = FALSE)
m <- mean( ~ duration, data = geyser)
s <-   sd( ~ duration, data = geyser)
ml.faithful <- maxLik(loglik.faithful, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml.faithful)
f <- function(a) {
  ml.faithful.a <- maxLik(loglik.faithful, x = geyser$duration,
                start = c(a, m - 1, m + 1, s, s), 
                fixed = 1)
  lrt.stat <- 2 * (logLik(ml.faithful) - logLik(ml.faithful.a)) 
  pval <- 1 - pchisq(lrt.stat, df = 1)         
  return(pval)
}
lo <- uniroot(function(a){f(a) - 0.05}, c(0.1, mle[1])) %>% value(); lo
hi <- uniroot(function(a){f(a) - 0.05}, c(0.9, mle[1])) %>% value(); hi

