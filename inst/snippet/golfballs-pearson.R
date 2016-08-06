# manual calculation
o <- golfballs; o
e <- rep(486 / 4, 4); e
X <- sum ((o - e)^2 / e); X
1 - pchisq(X, df = 3)
# repeated using built-in method
chisq.test(o)

