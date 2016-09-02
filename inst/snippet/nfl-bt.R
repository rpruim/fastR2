# fit Bradley-Terry model
require(BradleyTerry2)
NFL.model <- 
  BTm(cbind(homeTeamWon, !homeTeamWon), home, visitor, data = NFL, id = "team") 

