# note the different order of the arguments and 
# different output formats in these two functions.
quantile( ~ duration, data = MASS::geyser, probs = (0:10)/10)
qdata( ~ duration, (0:10)/10, data = MASS::geyser)
quantile( ~ duration, probs = (0:4)/4, data = MASS::geyser)
qdata( ~ duration, (0:4)/4, data = MASS::geyser)

