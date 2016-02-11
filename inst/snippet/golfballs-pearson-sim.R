E <- rep(486/4, 4)
chisqstat <- function(x) { sum((x-E)^2 / E) }
statTally(golfballs, rgolfballs, chisqstat, xlab = expression(X^2))

