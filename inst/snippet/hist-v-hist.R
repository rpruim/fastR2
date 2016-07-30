histogram( ~ Sepal.Length, data = iris)
hist(iris$Sepal.Length)
ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()

