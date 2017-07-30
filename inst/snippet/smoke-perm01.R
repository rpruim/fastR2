smokeTab <- 
  tally( ~ student + parents, data = FamilySmoking) 
smokeTab
chisq.test(smokeTab)
observedStat <- chisq.test(smokeTab) %>% stat(); observedStat
Stats <- do(2000) * {
  tally( ~ shuffle(student) + parents,  data = FamilySmoking) %>%
  chisq.test() %>%
  stat()
}
prop( ~ (X.squared >= observedStat), data = Stats) 
binom.test(0, 2000, alternative = "less") %>% confint()

