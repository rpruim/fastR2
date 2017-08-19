# fit a Bradley-Terry model
require(BradleyTerry2) 
NCAA.model <- 
  BTm(cbind(homeTeamWon, 1 - homeTeamWon), home, away, data = NCAA)

