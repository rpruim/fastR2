fisher.counts <- c(1997, 906, 904, 32)
# computes model probabilities from value of theta
theta2probs <- function(theta) {
    c(0.25 * (2 + theta), 
      0.25 * (1-theta), 
      0.25 * (1-theta), 
      0.25 * theta )
}

