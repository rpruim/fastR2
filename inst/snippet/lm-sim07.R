tally( ~ status, data = Sims2, format = "prop")
binom.test( ~ status, data = Sims2, p = 0.95)
chisq.test(tally( ~ status, data = Sims2), p = c(0.95, 0.025, 0.025))

