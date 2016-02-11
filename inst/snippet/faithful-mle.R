# seed the algorithm  
require(MASS)  # contains the geyser data set
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml <- 
  maxLik(loglik.faithful, x = geyser$duration,
    start = c(alpha = 0.5, mu1 = m-1, mu2 = m+1, sigma1 = s, sigma2 = s)) 
returnMessage(ml)
mle <- coef(ml); mle

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

