# find q such that Prob( X_1 <= q & X_2 <= q & X_3 <= q) = 0.5
mvtnorm::qmvnorm(0.5, mean = mu, sigma = Sigma)

