require(maxLik)
ml <- maxLik(loglik, start = c(shape1 = 1, shape2 = 1), x = ba)
ml
# get just the estimated parameter values
coef(ml)
# get just the "return message" -- always good to check
returnMessage(ml)

