gf_dhistogram( ~ boot.mean | sample, data = Boots, binwidth = 0.5) %>%
  gf_dist("norm", mean = 100, sd = 12 / sqrt(36))

