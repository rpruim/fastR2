# create the bootstrap distribution
Boys.boot <- 
  do(3000) * c(var.boot = var( ~ Height, data = resample(Boys10)))
# check for biased estimator
(mean( ~ var.boot, data = Boys.boot) - var( ~ Height, data = Boys10)) / 
  sd( ~ var.boot, data = Boys.boot)

