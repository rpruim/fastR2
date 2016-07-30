ml.fisher  <- maxLik(loglik.fisher,  start = 0.5, x = fisher.counts); ml.fisher
ml.fisher2 <- maxLik(loglik.fisher2, start = 0.5, x = fisher.counts); ml.fisher2
theta.hat <- coef(ml.fisher)

