m <- max( ~ week1, data = Fumbles)
xbar <- mean( ~ week1, data = Fumbles); xbar
Week1 <-
  data_frame(
    fumbles = 0:m,
    `observed count` = 
      as.vector(tally( ~ factor(week1, levels = 0:m), data = Fumbles)),
    `model count` = 120 * dpois(0:m, xbar)
  ) %>% 
  mutate(
    `observed pct` = 100 * `observed count` / 120,
    `model pct` = 100 * `model count` / 120
  )
Week1

