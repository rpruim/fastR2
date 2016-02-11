# works just fine if we select a good starting point.
summary(maxLik(loglik, start = 10, x = x))
# but some starting points don't work well...
p <- seq(7, 12, by = 0.2)
est <- sapply(p, function(p) { coef(maxLik(loglik, start = start, x = x)) })
rbind(p, est)
# here's another try without the logarithmic transformation.
est <- sapply(p, function(p) { coef(maxLik(lik, start = start, x = x)) })
rbind(p, est)

