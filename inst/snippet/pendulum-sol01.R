model <- lm(period ~ sqrt(length), data = Pendulum)
msummary(model)
confint(model)
f <- makeFun(model) 
plot(model, w = 1)
plot(model, w = 2)

