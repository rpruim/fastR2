x <- 
  c(57.9, 70.8, 86.3, 92.3, 94.2, 117.0, 118.4, 122.4, 125.8, 134.4)
n <- length(x)
mean(x)
sd(x)                    # NOT the method of moments estimate for sigma
sqrt(sum((x - mean(x))^2 / n))  # method of moments estimate for sigma
v <- 9/10 * var(x)       # working from var() and adjusting denominator
sqrt(v)

