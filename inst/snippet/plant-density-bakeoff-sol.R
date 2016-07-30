# have a bake-off:
summaryResults <- 
  results %>%
  group_by(size, lambda, method) %>%
  summarise(
    bias = mean(estimate) - lambda[1],
    biasR = mean(estimate) / lambda[1],
    se = sd(estimate)
  )
head(summaryResults)

