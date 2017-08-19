Versi <- iris %>% filter(Species == "versicolor")
testStat <- with(Versi, corStat(Sepal.Length, Petal.Length)); testStat
VersiSims <-
  expand.grid(rep = 1:10000) %>%
  group_by(rep) %>%
  mutate(simStat = with(Versi, corStat(Sepal.Length, shuffle(Petal.Length))))

# 1-sided p-value
prop1( ~ (simStat >= testStat), data = VersiSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = VersiSims)

