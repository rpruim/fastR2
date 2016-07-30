# for CI; p-value not interesting here
t.test( ~ Sepal.Width, data = iris %>% filter(Species == "virginica"))       
# this gives a more interesting p-value
t.test( ~ Sepal.Width, data = iris %>% filter(Species == "virginica"), mu = 3)   

