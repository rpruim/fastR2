xyplot(log(mass) ~ log(diameter), data = Spheres)
spheres.lm <- lm(log(mass) ~ log(diameter), data = Spheres)
confint(spheres.lm)
plot(spheres.lm, w = 1:2)

