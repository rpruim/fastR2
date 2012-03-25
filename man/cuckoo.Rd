\name{cuckoo}
\alias{cuckoo}
\docType{data}
\title{ Cuckoo eggs in other birds' nests }
\description{
Cuckoos are knows to lay their eggs in the nests of other (host) birds. 
The eggs are then adopted and hatched by the host birds.
These data were originally collected by O. M. Latter in 1902 to see how the size 
of a cuckoo egg is related to the species of the host bird.
}
\usage{data(cuckoo)}
\format{
  A data frame with 120 observations on the following 2 variables.
  \describe{
    \item{\code{length}}{length of egg (mm)}
    \item{\code{species}}{a factor with levels \code{hedge sparrow} \code{meadow pipet} \code{pied wagtail} \code{robin} \code{tree pipet} \code{wren}}
  }
}
\source{
L.H.C. Tippett, \emph{The Methods of Statistics}, 4th Edition, 
John Wiley and Sons, Inc., 1952, p. 176.
}
\references{
These data are also available from DASL, the data and story library
(\url{http://lib.stat.cmu.edu/DASL/}).
}
\examples{
data(cuckoo)
bwplot(length~species,cuckoo)
}
\keyword{datasets}
