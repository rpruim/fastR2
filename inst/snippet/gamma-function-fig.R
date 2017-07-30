inputs <- seq(0.05, 5, by = 0.05)
gf_point(gamma(1:5) ~ 1:5, shape = 1) %>%
gf_line(gamma(inputs) ~ inputs) %>%
  gf_lims(y = c(0, factorial(4))) %>%
  gf_labs(x = "x", y = expression(Gamma(x)))

