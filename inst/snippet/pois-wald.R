SE <- stdEr(ml10)
z.star <- qnorm(0.975)
1.4 + c(-1, 1) * z.star * SE

