# density function for mixture of normals
dmix <- function(x, alpha, mu1, mu2, sigma1, sigma2) {
    if (alpha < 0) return (dnorm(x, mu2, sigma2))
    if (alpha > 1) return (dnorm(x, mu1, sigma1))
    
    alpha * dnorm(x, mu1, sigma1) + (1-alpha) * dnorm(x, mu2, sigma2)
    }

# log-likelihood
loglik <- function(theta, x) {
    alpha <- theta[1]  
    mu1 <- theta[2]
    mu2 <- theta[3]
    sigma1 <- theta[4]
    sigma2 <- theta[5]
    density <- function (x) {
        if (alpha < 0) return (NA)
        if (alpha > 1) return (NA)
        if (sigma1 <= 0) return (NA)
        if (sigma2 <= 0) return (NA)
        dmix(x, alpha, mu1, mu2, sigma1, sigma2)
    }
    sum( log ( sapply( x, density) ) )
}

loglik0 <- function(theta, x) {
    theta <- c(0.5, theta)
    return(loglik(theta, x))
}

