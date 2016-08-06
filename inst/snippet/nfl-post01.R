bta <- BTabilities(NFL.model)
nflRatings<- data.frame(
    team = rownames(bta),
    rating = bta[, "ability"],
    se = bta[, "s.e."],
    wins = as.vector(tally( ~ winner, data = NFL)),
    losses = as.vector(tally( ~ loserr, data = NFL))
    )
rownames(nflRatings) = NULL

NFL <- NFL %>% 
  mutate(
    winnerRating = nflRatings$rating[as.numeric(winner)],
    loserRating  = nflRatings$rating[as.numeric(loser)], 
    upset = loserRating > winnerRating)

nflRatings[rev(order(nflRatings$rating)), ]

