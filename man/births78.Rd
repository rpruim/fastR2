\name{births78}
\alias{births78}
\docType{data}
\title{ Births by Day }
\description{The number of live births in the United States for each day
of the year 1978.}
\usage{data(births78)}
\format{
  A data frame with 365 observations on the following 3 variables.
  \describe{
    \item{\code{date}}{date (as a factor)}
    \item{\code{births}}{number of live births}
    \item{\code{dayofyear}}{number of days since start of year}
  }
}
\examples{
data(births78)
xyplot(births ~ dayofyear, births78)
}
\keyword{datasets}
