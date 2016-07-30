x <- SmallData$x; y <- SmallData$y
Sxx <- sum((x - mean(x))^2); Sxx
Sxy <- sum((x - mean(x)) * (y - mean(y))); Sxy
r <- 1 / 3 * sum( (x - mean(x)) / sd(x) * (y - mean(y)) / sd(y) ); r
slope <- r * sd(y) / sd(x); slope
intercept <- mean(y) - slope * mean(x); intercept

