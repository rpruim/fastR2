data.frame(
  x = 7:9,
  `P(X <= x)` =  pbinom(7:9, 25, 0.5),
  check.names = FALSE
)

