\name{rubberband}
\alias{rubberband}
\docType{data}
\title{ Rubber band launching -- unfinished documentation }
\description{
Results of an experiment comparing a rubber band travels to the amount it was 
stretched prior to launch.
}
\usage{data(rubberband)}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{Stretch}}{amount rubber band was stretched before launch}
    \item{\code{Distance}}{distance rubber band traveled }
  }
}

\examples{
data(rubberband)
xyplot(Distance ~ Stretch, rubberband, type=c("p","r"))
}
\keyword{datasets}
