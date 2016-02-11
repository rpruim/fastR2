# loglik defined above
snippet("loglik-faithful")
mle <- coef(maxLik(loglik, x = geyser$duration,
                   start = c(0.5, m - 1, m + 1, s, s)))
f <- function(a) {
    loglik0 <- function(theta, x) {
        theta <- c(a, theta)
        return(loglik(theta, x))              
    }
    mle0 <- coef(maxLik(loglik0, start = c(m-1, m+1, s, s), x = geyser$duration))
    stat <- 2 * (loglik(mle, x = geyser$duration) 
               - loglik0(mle0, x = geyser$duration)); stat
    pval <- 1 - pchisq(stat, df = 1)         
	return(pval)
}
lo <- uniroot( function(a){f(a) - 0.05}, c(0.1, mle[1]))$root; lo
hi <- uniroot( function(a){f(a) - 0.05}, c(0.9, mle[1]))$root; hi

