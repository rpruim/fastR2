data(iris)
Setosa <- iris %>% filter(Species == "setosa")
corStat <- function(x, y) {sum(x * y) - length(x) * mean(x) * mean(y)}

testStat <- with(Setosa, corStat(Sepal.Length, Petal.Length)); testStat
SetosaSims <-
  expand.grid(rep = 1:10000) %>%
  group_by(rep) %>%
  mutate(
    simStat = with(Setosa, corStat(Sepal.Length, shuffle(Petal.Length)))
  )
gf_dhistogram( ~ simStat, data = SetosaSims) %>%
  gf_vline(xintercept = testStat)
# 1-sided p-value
prop1( ~ (simStat >= testStat), data = SetosaSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = SetosaSims)

