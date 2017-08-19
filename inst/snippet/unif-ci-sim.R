Sims <-
  expand.grid(n = c(5, 10, 20, 50), rep = 1:10000, theta = 10) %>%
  group_by(n, rep, theta) %>%
  mutate(
    lower = max(runif(n, 0, theta)),
    upper = lower / 0.05^(1/n),
    cover = upper > theta
  )
prop(cover ~ n, data = Sims)

