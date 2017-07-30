# create the bootstrap distribution
Boys.boot.sd <- 
  do(3000) * c(sd.boot = sd( ~ Height, data = resample(Boys10)))
# check for biased estimator
(mean( ~ sd.boot, data = Boys.boot.sd) - sd( ~ Height, data = Boys10)) / 
  sd( ~ sd.boot, data = Boys.boot.sd)

