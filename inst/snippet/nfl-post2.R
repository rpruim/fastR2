NFL <- NFL %>% 
  mutate(pwinner = ilogit(winnerRating - loserRating))
# how big an upset was the Super Bowl?
NFL[nrow(NFL), ]

