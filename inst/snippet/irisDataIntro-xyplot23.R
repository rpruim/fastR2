xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris)
xyplot(Sepal.Length ~ Sepal.Width, groups = Species, 
       data = iris, cex = 1.3, alpha = .8, auto.key = list(columns = 3))

