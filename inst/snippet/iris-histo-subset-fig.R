histogram( ~ Sepal.Length | Species, data = iris,
    subset = Species == "virginica")
histogram( ~ Sepal.Length | ntiles(Sepal.Width, 4, format = "interval"), data = iris)

