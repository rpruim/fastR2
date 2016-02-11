plotFun(
  dunif(x) ~ x,
  xlim = c(-0.5, 1.5),
  ylim = c(-0.1, 1.2),
  main = "pdf for Unif(0,1)",
  xlab = "x",
  ylab = expression(f(x))
)

plotFun(
  punif(x) ~ x,
  xlim = c(-0.5, 1.5),
  ylim = c(-0.1, 1.2),
  main = "cdf for Unif(0,1)",
  xlab = "x",
  ylab = expression(F(x))
)

