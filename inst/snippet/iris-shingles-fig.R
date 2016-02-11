# histogram(~ Sepal.Length | Sepal.Width, data=iris)
histogram(~ Sepal.Length | equal.count(Sepal.Width, number = 4), data = iris)

