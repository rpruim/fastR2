# for CI; p-value not interesting here
t.test( ~Sepal.Width, data=subset(iris, Species=="virginica") )       
# this gives a more interesting p-value
t.test( ~ Sepal.Width, data=subset(iris, Species=="virginica"), mu=3)   

