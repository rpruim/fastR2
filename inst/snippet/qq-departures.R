dd <- rcauchy(50)
qqdata <- data.frame( 
	x = c(runif(100), rexp(100), rchisq(100, df = 2), dd, jitter(-dd)),
	dist = rep(c("A", "B", "C", "D"), each = 100) 
	)
xqqmath(~x | dist, data = qqdata,
		scales = list(relation = 'free', draw = FALSE),
		ylab = "data",
		xlab = "normal quantiles"
		)

