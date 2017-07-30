V <- iris %>% filter(Species == "virginica")
# for CI; p-value not interesting here -- mu = 0
t.test( ~ Sepal.Width, data = V)       
# this gives a more interesting p-value, if mu = 3 is an interesting claim
t.test( ~ Sepal.Width, data = V, mu = 3)

