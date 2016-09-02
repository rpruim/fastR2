ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
xyplot(time ~ height, data = BallDrop, type = c("p", "r"))
plot(ball.model, w = 1)

