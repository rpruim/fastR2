\name{pitching2005}
\alias{pitching2005}
\docType{data}
\title{
Major League Baseball 2005 pitching
}
\description{
Major League Baseball pitching statistics for the 2005 season.
}
\usage{data(pitching2005)}
\format{
  A data frame with 653 observations on the following 27 variables.
  \describe{
    \item{\code{playerID}}{unique identifier for each player}
    \item{\code{yearID}}{year}
    \item{\code{stint}}{for players who played with multiple teams in the same season,
	\code{stint} is increased by one each time the player joins a new team}
    \item{\code{teamID}}{three-letter identifier for team}
    \item{\code{lgID}}{league team plays in, coded as \code{AL} or \code{NL}}
    \item{\code{W}}{wins}
    \item{\code{L}}{losses}
    \item{\code{G}}{games played in}
    \item{\code{GS}}{games started} 
    \item{\code{CG}}{complete games}
    \item{\code{SHO}}{shut outs}
    \item{\code{SV}}{saves recorded}
    \item{\code{IPouts}}{outs recorded 
						(innings pitched, measured in outs rather than innings)}
    \item{\code{H}}{hits allowed}
    \item{\code{ER}}{earned runs allowed}
    \item{\code{HR}}{home runs allowed}
    \item{\code{BB}}{walks (bases on balls) allowed}
    \item{\code{SO}}{strike outs}
    \item{\code{BAOpp}}{opposing hitters' batting average}
    \item{\code{ERA}}{earned run average}
    \item{\code{IBB}}{intentional walks}
    \item{\code{WP}}{wild pitches}
    \item{\code{HBP}}{number of batters hit by pitch}
    \item{\code{BK}}{balks}
    \item{\code{BFP}}{batters faced pitching}
    \item{\code{GF}}{ratio of ground balls to fly balls}
    \item{\code{R}}{runs allowed}
  }
}
\examples{
data(pitching2005)
xyplot(IPouts/3 ~ W, pitching2005, ylab="innings pitched", xlab="wins")
}
\keyword{datasets}
