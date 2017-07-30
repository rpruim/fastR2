ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
gf_lm(time ~ height, data = BallDrop) %>%
  gf_point()
plot(ball.model, w = 1)

