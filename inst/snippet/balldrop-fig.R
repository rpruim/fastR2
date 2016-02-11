ball.model <- lm(time~height,balldrop)
###hop:3-9
summary(ball.model)
xyplot(time~height,balldrop,type=c('p','r'))
xplot(ball.model,w=1)

