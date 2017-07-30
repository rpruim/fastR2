gf_dist("beta", shape1 = 19, shape2 = 3, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(19, 3) posterior") %>%
  gf_lims(y = c(0, 6))
gf_dist("beta", shape1 = 10, shape2 = 2, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(10, 2) posterior") %>%
  gf_lims(y = c(0, 6))

