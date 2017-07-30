# normality check
gf_qq( ~ boot.mean, data = Dimes.boot)
SE <- sd( ~ boot.mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

