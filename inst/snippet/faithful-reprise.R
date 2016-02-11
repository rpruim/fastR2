snippet("dmix-faithful", echo = FALSE)
snippet("faithful-loglik", echo = FALSE)

loglik0.faithful <- function(theta, x) {
    theta <- c(0.5, theta)
    return(loglik(theta, x))
}

