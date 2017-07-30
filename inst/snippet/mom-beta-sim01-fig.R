Results <- do(1000) * beta.mom(rbeta(50, 2, 5))
gf_dhistogram( ~ shape1, data = Results, bins = 30) %>%
  gf_vline(xintercept = 2)
gf_dhistogram( ~ shape2, data = Results, bins = 30) %>%
  gf_vline(xintercept = 5)

