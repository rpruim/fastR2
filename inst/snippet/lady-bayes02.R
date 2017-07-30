posterior_sample20 <- rbeta(1e5, shape1 = 19, shape2 = 3)
posterior_sample10 <- rbeta(1e5, shape1 = 10, shape2 = 2)
gf_dhistogram( ~ posterior_sample20, binwidth = 0.01) %>%
  gf_labs(title = "Sampling from a Beta(19, 3) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))
gf_dhistogram( ~ posterior_sample10, binwidth = 0.01, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Sampling from a Beta(10, 2) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))

