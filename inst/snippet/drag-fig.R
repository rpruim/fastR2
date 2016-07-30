xyplot(velocity^2 ~ force.drag, drag, groups = height)
plot(model1, w = 1)
xyplot(velocity ~ force.drag, drag, scales = list(log = T), groups = height)
plot(model3, w = 1)

