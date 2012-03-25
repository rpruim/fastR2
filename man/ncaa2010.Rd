\name{ncaa2010}
\alias{ncaa2010}
\alias{ncaa2009}
\alias{ncaa2008}
\docType{data}
\title{NCAA Division I Basketball Results}

\description{
Results of NCAA basketball games
}
\usage{
data(ncaa2010)

data(ncaa2009)

data(ncaa2008)
}
\format{
  Seven variables describing NCAA Division I basketball games.
  \describe{
    \item{\code{date}}{date on which game was played}
    \item{\code{away}}{visiting team}
    \item{\code{ascore}}{visiting team's score}
    \item{\code{home}}{home team}
    \item{\code{hscore}}{home team's score}
    \item{\code{notes}}{code indicting games played at neutral sites (n or N) 
	or in tournaments (T)}
    \item{\code{location}}{where game was played}
  }
}
\source{ \url{kenpom.com} }
\examples{
data(ncaa2010)
# add some additional variables to the data frame
ncaa2010$dscore <- ncaa2010$hscore- ncaa2010$ascore
ncaa2010$homeTeamWon <- ncaa2010$dscore > 0
ncaa2010$numHomeTeamWon <- -1 + 2 * as.numeric(ncaa2010$homeTeamWon)
w <- which(ncaa2010$homeTeamWon)
ncaa2010$winner <- as.character(ncaa2010$away)
ncaa2010$winner[w] <- as.character(ncaa2010$home)[w]
ncaa2010$loser <- as.character(ncaa2010$home)
ncaa2010$loser[w] <- as.character(ncaa2010$away)[w]
ncaa2010$homeTeamWon <- ncaa2010$winner == ncaa2010$home
ncaa2010$numHomeTeamWon <- -1 + 2 * as.numeric(ncaa2010$homeTeamWon)
}
\keyword{datasets}
