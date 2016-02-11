qqmath(~Sepal.Length|Species,data=iris)
set.seed(1)                      # use fixed random seed
qqmath(~rnorm(150) | factor(rep(1:3,each=50)))

