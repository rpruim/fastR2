# density function for mixture of normals
dmix <- function(x, alpha, mu1, mu2, sigma1, sigma2) {
  if (alpha < 0 || alpha > 1) return (NA)
  if (sigma1 < 0 || sigma2 < 0) return (NA)
  alpha * dnorm(x, mu1, sigma1) + (1-alpha) * dnorm(x, mu2, sigma2)
}

