require(DAAG); data(elasticband)
eband.model <- lm(distance ~ stretch, data = elasticband)
ef <- eband.model$effects; n <- length(ef)
ef
# total length
sum(ef^2)
sum(elasticband$distance^2)
# projection of residuals into n-2 orthogonal components
sum(ef[3:n]^2)
sum(resid(eband.model)^2)
# projection in direction of u[0] is mean * sqrt(n)
mean(elasticband$distance) * sqrt(n)
ef[1]
# beta1.hat obtained from projection in direction of u[1]
# Note: R's u[1] points in the opposite direction.
ef[2] / sqrt(sum((elasticband$stretch - mean(elasticband$stretch))^2))
coef(eband.model)

