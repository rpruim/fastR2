favstats(yield ~ type, data = Study)
anova(lm(yield ~ type, data = Study))
group.means <- round(c(mean(yield ~ type, data = Study)), 1); group.means
y.bar <- 5 * sum(group.means) / 15; y.bar
group.sds <- round(sd(yield ~ type, data = Study), 3); group.sds
data_frame(
  SSE = sum(4 * group.sds^2), MSE = SSE / 12, 
  SSM = sum(5 * (group.means - y.bar)^2), MSM = SSM / 2, 
  F = MSM/MSE, p = 1 - pf(F, 2, 12))

