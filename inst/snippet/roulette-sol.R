val <- c(-1, 1)
prob <- c(20/38, 18/38)
sum( val * prob)                       # expected value
sum( val^2 * prob) - sum(val * prob)^2 # variance

