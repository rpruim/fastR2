# function to compute G statistic from tabulated data
G <- function(o) {e <- rep(486 / 4, 4); 2 * sum (o * log(o / e))}
statTally(golfballs, rgolfballs, G)  

