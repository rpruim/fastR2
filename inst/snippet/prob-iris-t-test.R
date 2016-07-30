with(iris, levels(Species))
with(iris, t.test(Sepal.Length[Species == levels(Species)[1]]))
with(iris, t.test(Sepal.Length[Species == levels(Species)[2]]))
with(iris, t.test(Sepal.Length[Species == levels(Species)[3]]))

