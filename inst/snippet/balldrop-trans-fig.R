ball.modelT <- lm(time ~ sqrt(height),balldrop)
summary(ball.modelT)
xyplot(time~height,balldrop, panel=panel.lm,model=ball.modelT)
xplot(ball.modelT,w=1)

