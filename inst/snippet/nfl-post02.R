NFL <- NFL %>% 
  mutate(
    winnerRating = nflRatings$rating[as.numeric(winner)],
    loserRating  = nflRatings$rating[as.numeric(loser)], 
    upset = loserRating > winnerRating,
    pwinner = ilogit(winnerRating - loserRating))
# how big an upset was the Super Bowl?
NFL %>% tail(1)

