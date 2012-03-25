\name{balldrop}
\alias{balldrop}
\docType{data}
\title{ Ball dropping data } 
\description{
Undergraduate students in a physics lab recorded the height from
which a ball was dropped and the time it took to reach the floor.
}
\usage{data(balldrop)}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{height}}{height in meters}
    \item{\code{time}}{time in seconds}
  }
}
\source{
Steve Plath, Calvin College Physics Department
}
\examples{
xyplot(time ~ height, balldrop)
}
\keyword{datasets}
