ml0a <- maxLik(loglik.faithful, x = geyser$duration,
            start = c(0.5, m - 1, m + 1, s, s),
            fixed = 1)       # first parameter is fixed at start value
coef(ml0a)
logLik(ml0a)

