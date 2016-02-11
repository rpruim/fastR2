# We can express l() in terms of sufficient statistics 
l <- function(theta, n = 10, x.bar = 1.4) {
  - n * theta + n * x.bar * log(theta)
}

ml <- maxLik2(l, start = c(lambda = 1), n = 10, x.bar = 1.4)
plot(ml)

