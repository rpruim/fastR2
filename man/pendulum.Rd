\name{pendulum}
\alias{pendulum}
\docType{data}
\title{ Pendulum data }
\description{
Period and pendulum length for a number of string and mass pendulums
constructed by physics students.  The same mass was used throughout, but
the length of the string was varied from 10cm to 16 m.
}
\usage{data(pendulum)}
\format{
  A data frame with 27 observations on the following 3 variables.
  \describe{
    \item{\code{length}}{length of the pendulum (in meters)}
    \item{\code{period}}{average time of period (in seconds) over several 
	swings of the pendulum}
    \item{\code{delta.length}}{an estimate of the accuracy of 
	the length measurement}
  }
}
\source{
Calvin College physics students under the direction of Professor Steve Plath.
}
\examples{
data(pendulum)
xyplot(period ~ length, pendulum)
}
\keyword{datasets}
