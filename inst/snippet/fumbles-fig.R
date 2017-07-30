gf_dhistogram( ~ week1, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
  gf_dist("pois", lambda = mean( ~ week1, data = Fumbles) )

