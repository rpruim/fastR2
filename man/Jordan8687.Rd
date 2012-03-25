\name{Jordan8687}
\alias{Jordan8687}
\docType{data}
\title{Michael Jordan personal scoring }
\description{
The number of points scored by Michael Jordan in each game 
of the 1986-87 regular season.}
\usage{data(Jordan8687)}
\format{
  A data frame with 82 observations on the following 2 variables.
  \describe{
    \item{\code{Game}}{a numeric vector}
    \item{\code{Points}}{a numeric vector}
  }
}
\examples{
data(Jordan8687)
xqqmath(~Points,data=Jordan8687)
}
\keyword{datasets}
