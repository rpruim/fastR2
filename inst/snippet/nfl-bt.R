# fit Bradley-Terry model
require(BradleyTerry2)
BTm(cbind(homeTeamWon, !homeTeamWon), home, visitor, 
          data = NFL, id = 'team') -> NFL.model

