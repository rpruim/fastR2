BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
msummary(ball.modelA)
gf_point(time ~ height, data = BallDropAvg) %>%
  gf_lm(formula = y ~ sqrt(x), fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelA, w = 1)

