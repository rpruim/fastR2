model2 <- lm(log(period) ~ log(length), data = Pendulum)
msummary(model2)
g <- makeFun(model2) 
plot(model2, w = 1)
plot(model2, w = 2)

