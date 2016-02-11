# test a specific value of theta vs best possible theta
testTheta <- function(theta, x) {
    chisqStat <- 2 * (loglik(theta.hat, x) - loglik(theta, x))
    chisqStat1 <- 2 * (loglik1(theta.hat, x) - loglik1(theta, x))
    p.value <- 1 - pchisq(chisqStat, df = 1)
    return( c(statistic = chisqStat, statistic1 = chisqStat1, 
              p.value = p.value))
    }
chisq.test(counts, p = theta2probs(coef(ml)))
# so we can grab that statistic and redo the p-value:
X <- stat(chisq.test(counts, p = theta2probs(coef(mle))))
1 - pchisq(X, df = 3 - 1)  # df = 3 for multinomial, 1 for model based on theta

