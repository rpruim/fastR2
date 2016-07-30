set.seed(12345)
Intervals <- 
  do(20) * 
  confint(t.test( ~ PTSG, data = sample(MIAA05, 15), conf.level = .90))
mu <- mean( ~ PTSG, data = MIAA05)
tally( ~ (lower <= mu & mu <= upper), data = Intervals)

