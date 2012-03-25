\name{familySmoking}
\alias{familySmoking}
\docType{data}
\title{ Family smoking }
\description{
A cross-tabulation of whether a student smokes and how many of his or her parents smoke
from a study conducted in the 1960's.
}
\usage{data(familySmoking)}
\format{
  A data frame with 5375 observations on the following 2 variables.
  \describe{
    \item{\code{Student}}{a factor with levels \code{DoesNotSmoke} \code{Smokes}}
    \item{\code{Parents}}{a factor with levels \code{NeitherSmokes} \code{OneSmokes}}
	\code{BothSmoke} 
  }
}
\source{
S. V. Zagona (ed.), \emph{Studies and issues in smoking behavior}, University of 
Arizona Press, 1967.
}
\references{
The data also appear in 

Brigitte Baldi and David S. Moore, \emph{The Practice of Statistics in the Life Sciences}, 
Freeman, 2009. 
}
\examples{
data(familySmoking)
xchisq.test( xtabs(~Parents + Student, familySmoking) )
}
\keyword{datasets}
