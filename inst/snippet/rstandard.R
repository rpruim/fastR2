SmallData <- data.frame(x = c(1, 2, 3, 4), y = c(2, 5, 6, 8))
small.model <- lm(y ~ x, data = SmallData)
# standardized residual computing manually
e <- resid(small.model); e
sigma.hat <- sqrt(sum(e^2) / 2);   sigma.hat       # internal estimate
h <- hatvalues(small.model); h
e / (sigma.hat * sqrt(1 - h))
# standardized residuals using rstandard()
rstandard(small.model)

