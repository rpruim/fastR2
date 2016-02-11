bta <- BTabilities(NFL.model)
nflRatings<- data.frame(
    team = rownames(bta),
    rating = bta[, "ability"],
    se = bta[, "s.e."],
    wins = as.vector(table(NFL$winner)),
    losses = as.vector(table(NFL$loser))
    )
rownames(nflRatings) = NULL

NFL <- NFL %>% 
  mutate(
    winnerRating = nflRatings$rating[as.numeric(winner)],
    loserRating  = nflRatings$rating[as.numeric(loser)], 
    upset = loserRating > winnerRating)

nflRatings[rev(order(nflRatings$rating)), ]

