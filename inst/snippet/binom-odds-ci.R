loglik.binom2 <- function(theta, x, n) {
  x * log(theta/(1 + theta)) + (n-x) *log(1/(1 + theta))
}
ml.binom2 <- maxLik2(loglik.binom2, start = (odds = 1), x = 35, n = 55)
coef(ml.binom2)
x <- 35; n <- 55; theta.hat <- 35/20; theta.hat
lo2 <- uniroot( 
  function(theta0) 
    2 * (loglik.binom2(theta.hat, x, n) - loglik.binom2(theta0, x, n)) - 
    qchisq(.95, df = 1), 
  c(0, theta.hat))$root
hi2 <- uniroot( 
  function(theta0) 
    2 * (loglik.binom2(theta.hat, x, n) - loglik.binom2(theta0, x, n)) - 
    qchisq(.95, df = 1), 
  c(theta.hat, 100))$root
c(lo2, hi2)
c(lo2, hi2) / (1 + c(lo2, hi2))
c(lo, hi)   # interval computed previously, for comparison
plot(ml.binom2) +
  geom_abline(slope = 0, intercept = logLik(ml.binom2) - 1.96, linetype = "dashed") +
  ylim(-45, -35)

