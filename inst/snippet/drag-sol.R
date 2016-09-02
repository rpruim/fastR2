model1 <- lm(velocity^2 ~ force.drag, data = Drag)
model2 <- lm(velocity ~ sqrt(force.drag), data = Drag)
model3 <- lm(log(velocity) ~ log(force.drag), data = Drag)
msummary(model1)
msummary(model2)
msummary(model3)

