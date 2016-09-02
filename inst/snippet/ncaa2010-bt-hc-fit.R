require(BradleyTerry2)
# home team gets advantage unless on neutral court
NCAA$homeTeam <- data.frame(team = NCAA$home, at.home = 1 - NCAA$neutralSite)
NCAA$awayTeam <- data.frame(team = NCAA$away, at.home = 0)
NCAA.model2 <- 
  BTm(cbind(homeTeamWon, 1-homeTeamWon),
      homeTeam, awayTeam, id = "team", formula = ~ team + at.home, data = NCAA)

