# direct calculation
loglik.fisher <- function(theta, x) {
  if (theta < 0 || theta > 1) return(NA)
  (  x[1]         * log(0.25 * (2 + theta)) 
  + (x[2] + x[3]) * log(0.25 * (1 - theta))
  +  x[4]         * log(0.25 * theta)
  )
}
# using dmultinom()
loglik.fisher2 <- function(theta, x) {
    if (theta < 0 || theta > 1) { return (NA) }
    dmultinom(x, size = sum(x), prob = theta2probs(theta), log = TRUE)
}


