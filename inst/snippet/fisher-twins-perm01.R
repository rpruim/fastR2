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

