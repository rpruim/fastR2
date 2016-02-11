NFL <- NFL2007                         # shorten name of data set
head(NFL, 3)
NFL <- NFL %>% mutate(
  dscore = homeScore - visitorScore,
  winner = ifelse(dscore > 0, home, visitor),
  loser  = ifelse(dscore > 0, visitor, home),
  homeTeamWon = dscore > 0
  )
head(NFL, 3)

