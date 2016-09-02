ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
xyplot(time ~ height, data = BallDrop, panel = panel.lm, model = ball.modelT)
plot(ball.modelT, w = 1)

