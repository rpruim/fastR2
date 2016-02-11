loglik <- function(theta, x) { 
    probs <- c(theta, 1-sum(theta))
    if (any (probs < 0)) return(NA)
    return( dmultinom(x, size = 100, prob = probs, log = TRUE) )
    }
maxLik(loglik, start = rep(0.25, 3), x = c(10, 20, 30, 40)) -> ml; 
coef(ml)

