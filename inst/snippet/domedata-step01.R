dome.lm1 <- 
  lm(Dist ~ Velocity + Angle + BallWt + BallDia + Cond, data = domedata)
step(dome.lm1, direction = "both", trace = FALSE)

