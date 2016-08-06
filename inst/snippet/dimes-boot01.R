x.bar <- mean( ~ mass, data = Dimes); x.bar
Dimes.boot <- do(5000) * mean( ~ mass, data = resample(Dimes))
histogram( ~ mean, data = Dimes.boot)

