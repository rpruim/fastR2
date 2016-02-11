# simulate 100 intervals and plot them.
CIs100 <- CIsim(n=20, samples=100, estimand=500, 
	rdist=rnorm, args=list(mean=500, sd=100),
	method=ci, method.args=list(sd=100))
require(Hmisc)
xYplot(Cbind(estimate, lower, upper) ~ sample, data=CIs100,
	groups=cover, col=c('black', 'gray40'), cap=0, lwd=2, pch=16)

