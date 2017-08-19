ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
gf_point(time ~ height, data = BallDrop) %>%
  gf_lm(formula = y ~ sqrt(x), interval = "prediction", 
             fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelT, w = 1)

