p <- seq(0, 1, by = 0.01)
power <- 1 - (pbinom(60, 100, p) - pbinom(39, 100, p))
gf_line(power ~ p, size = 1) %>%
  gf_labs(x = expression(pi[a]))

