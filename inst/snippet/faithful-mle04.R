# seed the algorithm  
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

# Newton-Raphson (NR) compares well to the results above
maxLik(loglik.faithful,  x = geyser$duration,
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s)) 
# Nelder-Mead doesn't converge (fast enough)
maxLik(loglik.faithful, x = geyser$duration, method = "NM",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s)) 
# Nelder-Mead converges if we give it more time
maxLik(loglik.faithful, x = geyser$duration, method = "NM", 
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s), 
       control = list(iterlim = 3000))
# BFGS "converges", but only fits one group
maxLik(loglik.faithful, x = geyser$duration, method = "BFGS",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s))

