\name{nfl2007}
\alias{nfl2007}
\docType{data}
\title{ NFL 2007 season }

\description{
Results of National Football League games (2007 season, including playoffs)
}
\usage{data(nfl2007)}
\format{
  A data frame with 267 observations on the following 7 variables.
  \describe{
    \item{\code{Date}}{date on which game was played}
    \item{\code{Visitor}}{visiting team}
    \item{\code{VisitorScore}}{score for visiting team}
    \item{\code{Home}}{home team}
    \item{\code{HomeScore}}{score for home team}
    \item{\code{Line}}{`betting line'}
    \item{\code{TotalLine}}{'over/under' line (for combined score of both teams)}
  }
}
\examples{
data(nfl2007); nfl <- nfl2007
nfl$dscore <- nfl$HomeScore - nfl$VisitorScore 
w <- which(nfl$dscore > 0) 
nfl$winner <- nfl$Visitor; nfl$winner[w] <- nfl$Home[w] 
nfl$loser <- nfl$Home; nfl$loser[w] <- nfl$Visitor[w] 
# did the home team win? 
nfl$homeTeamWon <- nfl$dscore > 0 
table(nfl$homeTeamWon)
table(nfl$dscore > nfl$line)
}
\keyword{datasets}
