# these two methods give different numerical values
AIC(concrete.lm0)
AIC(concrete.lm1)
extractAIC(concrete.lm0)
extractAIC(concrete.lm1)
# but differences between models are equivalent
AIC(concrete.lm0) - AIC(concrete.lm1)
extractAIC(concrete.lm0)[2] - extractAIC(concrete.lm1)[2]

