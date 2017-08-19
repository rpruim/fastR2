x <- seq(0, 1, by = 0.01)
gf_line( x * (1-x) ~ x, size = 1) %>%
  gf_labs(title = "Variance of a Bernoulli random variable",
    x = expression(pi), y = expression(Var(X)) )

