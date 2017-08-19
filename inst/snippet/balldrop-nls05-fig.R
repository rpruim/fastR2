f <- makeFun(balldrop.nls)
gf_point( time ~ height, data = BallDrop ) %>%
  gf_fun(f(height) ~ height, alpha = 0.4)
plot(balldrop.nls)
gf_point(resid(balldrop.nls) ~ fitted(balldrop.nls))  # unstandardized resids

