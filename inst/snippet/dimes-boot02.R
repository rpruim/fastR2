# normality check
xqqmath( ~ mean, data = Dimes.boot)
SE <- sd( ~ mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

