\name{buckthorn}
\alias{buckthorn}
\docType{data}
\title{ Buckthorn }
\description{
Data from an experiment to determine the efficacy of various methods
of eradicating buckthorn, an invasive woody shrub.  Buckthorn plants
were chopped down and the stumps treated with various concentrations 
of glyphosate.  The next season, researchers returned to see whether
the plant had regrown.
}
\usage{data(buckthorn)}
\format{
  A data frame with 165 observations on the following 3 variables.
  \describe{
    \item{\code{shoots}}{number of new shoots coming from stump}
    \item{\code{conc}}{concentration of glyphosate applied}
    \item{\code{dead}}{weather the stump was considered dead}
  }
}
\source{David Dornbos, Calvin College}
\examples{
data(buckthorn)
}
\keyword{datasets}
