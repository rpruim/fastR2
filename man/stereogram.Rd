\name{stereogram}
\alias{stereogram}
\docType{data}
\title{ Stereogram fusion }
\description{
Results of an experiment on the effect of prior information on the time to
fuse random dot steregrams. One group (NV) was given either no information
 or just verbal information about the shape of the embedded object. A second
 group (group VV) received both verbal information and visual information
 (e.g., a drawing of the object).  
}
\usage{data(stereogram)}
\format{
  A data frame with 78 observations on the following 2 variables.
  \describe{
    \item{\code{Time}}{time until subject was able to fuse a random dot stereogram}
    \item{\code{Group}}{treatment group: \code{NV}(no visual instructions) \code{VV} (visual instructions)}
  }
}
\source{These data are available at DASL, the data and story library (\url{http://lib.stat.cmu.edu/DASL/}.
}
\references{
Frisby, J. P.  and Clatworthy, J. L., "Learning to see complex random-dot stereograms,"
\emph{Perception}, 4, (1975), pp. 173-178.

Cleveland, W. S. \emph{Visualizing Data}. 1993.
}
\examples{
data(stereogram)
require(Hmisc)
summary(Time~Group, stereogram,fun=favstats)
}
\keyword{datasets}
