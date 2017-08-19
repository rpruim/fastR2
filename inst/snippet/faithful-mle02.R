# log-likelihood
loglik.faithful <- function(theta, x) {
  alpha <- theta[1]  
  mu1 <- theta[2]; mu2 <- theta[3]
  sigma1 <- theta[4]; sigma2 <- theta[5]
  
  sum(log(dmix(x, alpha, mu1, mu2, sigma1, sigma2)))
}

