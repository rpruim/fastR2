qbeta(c(0.025, 0.975), 38+1, 62+1)
confint(binom.test(38, 100))            # for comparison
confint(prop.test(38, 100))             # for comparison

