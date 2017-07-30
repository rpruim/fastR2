gf_histogram( ~ Sepal.Length, data = iris)
# manually selecting width of bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5)
# also selecting the boundary of the bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5, boundary = 8)
# manually selecting number of bins
gf_histogram( ~ Sepal.Length, data = iris, bins = 15)

