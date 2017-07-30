tally( ~ status, data = Sims3) / 5000
binom.test( ~ status, data = Sims3, p = 0.95)
chisq.test(tally( ~ status, data = Sims3), p = c(0.95, 0.025, 0.025))

