\name{mlb2004}
\alias{mlb2004}
\docType{data}
\title{ Major League Baseball 2004 team data }
\description{
Team batting statistics, runs allowed, and runs scored for the 2004 Major League
Baseball season.
}
\usage{data(mlb2004)}
\format{
  A data frame with 30 observations on the following 20 variables.
  \describe{
    \item{\code{Team}}{team city, a factor}
    \item{\code{League}}{League, a factor with levels \code{AL} \code{NL}}
    \item{\code{W}}{number of wins}
    \item{\code{L}}{number of losses}
    \item{\code{G}}{number of games}
    \item{\code{R}}{number of runs scored}
    \item{\code{OR}}{oppnents' runs -- number of runs allowed}
    \item{\code{Rdiff}}{run difference -- \code{R - OR}}
    \item{\code{AB}}{number of at bats}
    \item{\code{H}}{number of hits}
    \item{\code{DBL}}{number of doubles}
    \item{\code{TPL}}{number of triples}
    \item{\code{HR}}{number of home runs}
    \item{\code{BB}}{number of walks (bases on balls)}
    \item{\code{SO}}{number of strike outs}
    \item{\code{SB}}{number of stolen bases}
    \item{\code{CS}}{number of times caught stealing}
    \item{\code{BA}}{batting average}
    \item{\code{SLG}}{slugging percentage}
    \item{\code{OBA}}{on base average}
  }
}
\examples{
data(mlb2004)
xyplot(W ~ Rdiff, mlb2004)
}
\keyword{datasets}
