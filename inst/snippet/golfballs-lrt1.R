# LRT calculation
o <- golfballs; o  
e <- rep(486 / 4, 4); e
G <- 2 * sum (o * log(o / e)); G       # lrt Goodness of fit statistic
1 - pchisq(G, df = 3)

