xyplot(velocity^2 ~ force.drag, data= Drag, groups = height)
plot(model1, w = 1)
xyplot(velocity ~ force.drag, data = Drag, 
       scales = list(log = T), groups = height)
plot(model3, w = 1)

