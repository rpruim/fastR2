qbeta(c(0.025, 0.975), 20+1, 30+1)
confint(binom.test(20, 50))             # for comparison
confint(prop.test(20, 50))              # for comparison

