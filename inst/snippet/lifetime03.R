favstats(life01)
n <- length(life01); x.bar <- mean(life01); s <- sd(life01)
Life.boot <-
  Life.boot %>%
  mutate(t = (mean - x.bar)/(sd/sqrt(n)))
q <- cdata( ~ t, data = Life.boot, p = 0.95); q
x.bar - q[2:1] * s/sqrt(n)

