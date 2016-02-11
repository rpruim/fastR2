xyplot(velocity^2 ~ force.drag, drag, groups=height)
xplot(model1,w=1)
xyplot(velocity ~ force.drag, drag, scales=list(log=T),groups=height)
xplot(model3,w=1)

