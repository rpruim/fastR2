# using v1 gives coefficient in model with only limestone as a predictor
dot(y, v1) / vlength(v1)^2
coef(lm(strength ~ limestone, data = concretemod))
# using v2 gives coefficient in model with only water as a predictor
dot(y, v2) / vlength(v2)^2
coef(lm(strength ~ water, data = concretemod))
# using w1 and w2 gives coefficients in the model 
dot(y, w1) / vlength(w1)^2
dot(y, w2) / vlength(w2)^2
coef(concrete.lmmod)

