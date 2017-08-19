# By the result above, this should be just under 0.5
mvtnorm::pmvnorm(upper = c(2, 2, 2), mean = mu, sigma = Sigma)
# Prob(all three are between -1 and 1)
mvtnorm::pmvnorm(lower = c(-1, -1, -1), upper = c(1, 1, 1), 
        mean = mu, sigma = Sigma)

