# method of moments estimates
# estimate for theta is the sample mean since E(X) == est.theta:
est.theta = mean(x); est.theta
# estimate for variance satisfies v == 2 / nest.lambda^2:
n <- length(x)
v <- var(x) * (n-1) / n
est.lambda <- sqrt(2 / v); est.lambda

