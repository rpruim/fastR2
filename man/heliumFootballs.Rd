\name{heliumFootballs}
\alias{heliumFootballs}
\docType{data}
\title{ Punting helium- and air-filled footballs }
\description{
Two identical footballs, one air-filled and one helium-filled, were 
used outdoors on a windless day at The Ohio State University's athletic 
complex. Each football was kicked 39 times and the two footballs were
alternated with each kick. The experimenter recorded the distance traveled by
each ball.
}
\usage{data(heliumFootballs)}
\format{
  A data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{Trial}}{trial number}
    \item{\code{Air}}{distance traveled by air-filled football (yards)}
    \item{\code{Helium}}{distance traveled by helium-filled football (yards)}
  }
}
\source{
These data are available from DASL, the data and story library (\url{http://lib.stat.cmu.edu/DASL/}).
}
\references{
Lafferty, M. B. (1993), 
"OSU scientists get a kick out of sports controversy", 
\emph{The Columbus Dispatch} (November, 21, 1993), B7.
}
\examples{
data(heliumFootballs)
xyplot(Helium~Air, data=heliumFootballs)
bwplot(~(Helium-Air), data=heliumFootballs)
}
\keyword{datasets}
