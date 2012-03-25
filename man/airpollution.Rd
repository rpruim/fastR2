\name{airpollution}
\alias{airpollution}
\docType{data}
\title{ Air pollution measurements }
\description{
Air pollution measurements at three locations.
}
\usage{data(airpollution)}
\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{pollution}}{a numeric vector}
    \item{\code{location}}{a factor with levels \code{Hill Suburb} \code{Plains Suburb} \code{Urban City}}
  }
}
\source{
David J. Saville and Graham R. Wood, \emph{Statistical methods: A geometric 
primer}, Springer, 1996. 
}
\examples{
data(airpollution)
summary(lm(pollution ~ location, airpollution))
}
\keyword{datasets}
