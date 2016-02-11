require(plyr)
balldropavg <- ddply( balldrop, .(height), summarise,
					 time =mean(time) )
balldropavg
ball.modelA <- lm(time ~ sqrt(height),balldropavg)
summary(ball.modelA)
xyplot(time~height,balldropavg,
                panel=panel.lm,model=ball.modelA)
xplot(ball.modelA,w=1)

