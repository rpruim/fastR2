NFL <- NFL2007 %>% mutate(
  dscore = homeScore - visitorScore,
  winner = ifelse(dscore > 0, home, visitor),
  loser  = ifelse(dscore > 0, visitor, home),
  homeTeamWon = dscore > 0
  )
head(NFL, 3)

