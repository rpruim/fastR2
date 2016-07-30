pi.hat <- coef(ml.binom); pi.hat
odds.hat <- pi.hat / (1 - pi.hat); odds.hat
coef(ml.binom2)
log(odds.hat)
coef(ml.binom3)

