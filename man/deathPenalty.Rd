\name{deathPenalty}
\alias{deathPenalty}
\alias{deathPen}
\docType{data}
\title{ Death Penalty and Race }
\description{
A famous example of Simpson's paradox.
}
\usage{
data(deathPenalty)

data(deathPen)
}
\format{
  A data frame with 326 observations.  The factors are coded more succinctly
  in \code{deathPen}, but otherwise the data are the same.
  \describe{
    \item{\code{DeathPenalty}}{a factor with levels \code{Yes} \code{No}}
    \item{\code{Penalty}}{a factor with levels \code{Death} \code{Not}}
    \item{\code{Victim}}{a factor with levels \code{Black} \code{White} 
	(or \code{Bl} \code{Wh})}
    \item{\code{Defendant}}{a factor with levels \code{Black} \code{White}
	(or \code{Bl} \code{Wh})}
  }
}
\source{
Radelet, M. (1981). Racial characteristics and imposition of the 
death penalty. \emph{American Sociological Review}, 46:918--927. 
}
\examples{
xtabs(~Defendant+Penalty,deathPenalty)
xtabs(~Defendant+Victim+Penalty,deathPenalty)
}
\keyword{datasets}
