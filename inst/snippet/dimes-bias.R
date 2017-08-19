# estimate of bias
(mean( ~ boot.mean, data = Dimes.boot) - mean( ~ mass, data = Dimes)) /
  sd( ~ boot.mean, data = Dimes.boot)

