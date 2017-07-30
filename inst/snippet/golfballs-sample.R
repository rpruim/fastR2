rmultinom(1, prob = c(.3, .3, .2, .2), size = 486)
tally(resample(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4), 486))

