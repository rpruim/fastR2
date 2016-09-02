mass <- makeFun(spheres.lm) 
xyplot(mass ~ diameter, data = Spheres)
plotFun(mass(x) ~ x, add = TRUE, lwd = 2, under = TRUE)

