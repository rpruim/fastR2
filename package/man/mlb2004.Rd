\docType{data}
\name{mlb2004}
\alias{mlb2004}
\title{Major League Baseball 2004 team data}
\format{A data frame with 30 observations on the following 20 variables.
\itemize{ \item{Team}{ team city, a factor}
\item{League}{ League, a factor with levels \code{AL} \code{NL}}
\item{W}{ number of wins} \item{L}{ number of losses}
\item{G}{ number of games} \item{R}{ number of runs scored}
\item{OR}{ oppnents' runs -- number of runs allowed}
\item{Rdiff}{ run difference -- \code{R - OR}}
\item{AB}{ number of at bats} \item{H}{ number of hits}
\item{DBL}{ number of doubles} \item{TPL}{ number of triples}
\item{HR}{ number of home runs} \item{BB}{ number of walks
(bases on balls)} \item{SO}{ number of strike outs}
\item{SB}{ number of stolen bases} \item{CS}{ number of times
caught stealing} \item{BA}{ batting average}
\item{SLG}{ slugging percentage} \item{OBA}{ on base average}
}}
\description{
  Team batting statistics, runs allowed, and runs scored
  for the 2004 Major League Baseball season.
}
\examples{
data(mlb2004)
xyplot(W ~ Rdiff, mlb2004)
}
\keyword{datasets}

