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
    sum(log(sapply(x, density)))
}

# seed the algorithm  
require(MASS)  # contains the geyser data set
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml <- 
  maxLik(loglik, x = geyser$duration,
    start = c(alpha = 0.5, mu1 = m-1, mu2 = m+1, sigma1 = s, sigma2 = s)) 
returnMessage(ml)
mle <- coef(ml); mle

d <- function(x) { 
    dmix(x, mle[1], mle[2], mle[3], mle[4], mle[5])
}

histogram( ~ duration, data = geyser,
    width = 0.25,
    density = TRUE,
    dmath = dmix,
    args = list(
        alpha =  mle[1],
        mu1 =    mle[2],
        mu2 =    mle[3],
        sigma1 = mle[4],
        sigma2 = mle[5])
    )

