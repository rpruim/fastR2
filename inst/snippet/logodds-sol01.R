loglik.binom3 <- function(theta, x, n) { 
  x * log(exp(theta)/(1 + exp(theta))) + (n-x) *log(1/(1 + exp(theta)))
}
ml.binom3 <- maxLik2( loglik.binom3, x = 35, n = 55, start = c(logodds = 0))
logodds.hat <- coef(ml.binom3); logodds.hat
p <- function(logodds) {
  2 * (loglik.binom3(coef(ml.binom3), x=35, n=55) - loglik.binom3(logodds, x=35, n=55))
}
lo <- uniroot( function(logodds) p(logodds) - 0.5, c(-100, logodds.hat))
hi <- uniroot( function(logodds) p(logodds) - 0.5, c( 100, logodds.hat))
c(lo$root, hi$root)

