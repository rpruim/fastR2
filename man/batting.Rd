\name{batting}
\alias{batting}
\docType{data}
\title{ Major League Batting 2000-2005}
\description{
Major League batting data for the seasons from 2000-2005.
}
\usage{data(batting)}
\format{
  A data frame with 8062 observations on the following 22 variables.
  \describe{
    \item{\code{player}}{unique identifier for each player}
    \item{\code{year}}{year}
    \item{\code{stint}}{for players who were traded mid-season, indicates
	which portion of the season the data cover}
    \item{\code{team}}{three-letter code for team}
    \item{\code{league}}{a factor with levels \code{AA} \code{AL} \code{NL}}
    \item{\code{G}}{games}
    \item{\code{AB}}{at bats}
    \item{\code{R}}{runs}
    \item{\code{H}}{hits}
    \item{\code{H2B}}{doubles}
    \item{\code{H3B}}{triples}
    \item{\code{HR}}{home runs}
    \item{\code{RBI}}{runs batted in}
    \item{\code{SB}}{stolen bases}
    \item{\code{CS}}{caught stealing}
    \item{\code{BB}}{bases on balls (walks)}
    \item{\code{SO}}{strike outs}
    \item{\code{IBB}}{intentional base on balls}
    \item{\code{HBP}}{hit by pitch}
    \item{\code{SH}}{a numeric vector}
    \item{\code{SF}}{sacrifice fly}
    \item{\code{GIDP}}{grounded into double play}
  }
}
\examples{
data(batting)
histogram(~HR,batting)
}
\keyword{datasets}
