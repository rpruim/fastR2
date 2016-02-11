theta2probs <- function(theta) { 
    c(theta^2, 2*theta*(1-theta), (1-theta)^2)  
}
loglik <- function(theta, x) {
  probs <- theta2probs(theta)
  if (any( probs < 0) ) return (-Inf)
	dmultinom( x, sum(x), theta2probs(theta), log = TRUE )
}

geno<-c(83, 447, 470)
ml <- maxLik(loglik, start = 0.5, x = geno)
summary(ml)
theta.hat <- coef(ml)

chisq.test(geno, start = theta2probs(theat.hat))
# so we can grab that statistic and redo the p-value:
X <- stat(chisq.test(geno, start = theta2probs(coef(ml))))
1 - pchisq(X, df = 2-1)  # df = 2 for multinomial, 1 for model based on theta

