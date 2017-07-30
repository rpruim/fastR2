# seed the algorithm  
data(geyser, package = "MASS")
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml.faithful <- 
  maxLik(loglik.faithful, x = geyser$duration,
    start = c(alpha = 0.5, mu1 = m - 1, mu2 = m + 1, sigma1 = s, sigma2 = s)) 
returnMessage(ml.faithful)
mle <- coef(ml.faithful); mle

gf_dhistogram( ~ duration, data = geyser, binwidth = 0.20, alpha = 0.5) %>%
  gf_function(fun = dmix, 
              args = list(
                alpha =  mle[1],
                mu1 =    mle[2], mu2 =    mle[3],
                sigma1 = mle[4], sigma2 = mle[5])
  )

