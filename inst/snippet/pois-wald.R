SE <- stdEr(ml.pois10); SE
z.star <- qnorm(0.975); z.star
1.4 + c(-1, 1) * z.star * SE

