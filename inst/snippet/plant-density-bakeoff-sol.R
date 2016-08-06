# have a bake-off:
SummaryResults <- 
  Results %>%
  group_by(size, lambda, method) %>%
  summarise(
    bias = mean(estimate) - lambda[1],
    biasR = mean(estimate) / lambda[1],
    se = sd(estimate)
  )
head(SummaryResults)

