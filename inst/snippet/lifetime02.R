Life.boot <- do(5000) * favstats(resample(life01))
cdata(~mean, data = Life.boot, p = 0.95)
# normalized bias
(mean( ~ mean, data = Life.boot) - x.bar) / sd( ~ mean, data = Life.boot)

