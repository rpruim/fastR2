\name{tirewear}
\alias{tirewear}
\docType{data}
\title{ Estimating tirewear }
\description{
Treadwear is estimated by two methods: weight loss and groove wear.
}
\usage{data(tirewear)}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{weight}}{estimated wear (1000's of miles) base on weight loss}
    \item{\code{groove}}{estimated wear (1000's of miles) based on groove wear}
  }
}
\source{
These data are available at DASL, the Data and Story Library (\url{http://lib.stat.cmu.edu/DASL/}).
}
\references{
R. D. Stichler, G. G. Richey, and J. Mandel, "Measurement of Treadware of Commercial
Tires", \emph{Rubber Age}, 73:2 (May 1953).
}
\examples{
data(tirewear)
xyplot(weight ~ groove, tirewear)
}
\keyword{datasets}
