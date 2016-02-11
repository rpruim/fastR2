theta2probs <- function(theta) { 
    c(theta^2, 2*theta*(1-theta), (1-theta)^2)  
}
loglik <- function(theta, x) {
  probs <- theta2probs(theta)
  if (any( probs < 0 )) { return( -Inf ) }
	dmultinom( x, sum(x), theta2probs(theta), log = TRUE )
}

geno<-c(83, 447, 470)
summary(maxLik(loglik, start = 0.5, x = geno))

