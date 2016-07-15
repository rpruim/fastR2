loglik.binom <- function(p, x, n) {
  if (p < 0 | p > 1) return (NA)  
  x * log(p) + (n-x) * log(1 - p) 
}
lo <- uniroot( 
  function(pi0) 2 * (loglik.binom(pi.hat, x, n) - loglik.binom(pi0, x, n)) - 
    qchisq(.95, df = 1), 
  c(0, pi.hat))$root
hi <- uniroot( 
  function(pi0) 2 * (loglik.binom(pi.hat, x, n) - loglik.binom(pi0, x, n)) - 
    qchisq(.95, df = 1), 
  c(pi.hat, 1))$root
c(lo, hi)

