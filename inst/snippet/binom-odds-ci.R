loglik.binom2 <- function(theta, x, n) {
  x * log(theta/(1 + theta)) + (n-x) *log(1/(1 + theta))
}
x <- 35; n <- 55; theta.hat <- 35/20
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
c(lo,hi)   # interval computed previously, for comparison

