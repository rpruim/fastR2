BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
summary(ball.modelA)
xyplot(time ~ height, BallDropAvg,
                panel = panel.lm, model = ball.modelA)
plot(ball.modelA, w = 1)

