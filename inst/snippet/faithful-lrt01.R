snippet("faithful-mle01", echo = FALSE) 
snippet("faithful-mle02", echo = FALSE)

loglik0.faithful <- function(theta, x) {
    theta <- c(0.5, theta)
    return(loglik.faithful(theta, x))
}

