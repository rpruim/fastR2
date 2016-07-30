numSims <- 20000 
FT <- 
  data.frame(
    twin = rep(c("Di", "Mono"), times = c(17, 13)),
    conviction = rep(c("No", "Yes", "No", "Yes"), times = c(15, 2, 3, 10))
  )
# check to see that table matches
tally(twin ~ conviction, data = FT)
# test statistic is value in top left cell
tally(twin ~ conviction, data = FT)[1, 1]

# simulated data sets
testStats <- replicate(numSims, {
    tally(twin ~ shuffle(conviction), data = FT)[1, 1]
    })

# for p-value 
tally(testStats)
# tail probabilities
prop1(testStats >= 15)
# 2-sided p-value
2 * prop1(testStats >= 15)

