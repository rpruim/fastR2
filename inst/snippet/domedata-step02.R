step(
  lm(Dist ~ 1, data = domedata),  # starting point
  scope = Dist ~ Velocity + Angle + BallWt + BallDia + Cond, 
  direction = "forward", trace = FALSE)

