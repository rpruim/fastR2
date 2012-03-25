\name{traffic}
\alias{traffic}
\docType{data}
\title{ New England traffic fatalities (1951-1959) }
\description{
Used by Tufte as an example of the importance of context, these data show the
traffic fatality rates in New England in the 1950s.  Connecticut increased
enforcement of speed limits in 1956.  In their full context, it is difficult
to say if the decline in Connecticut traffic fatalities from 1955 to 1956 can be 
attributed to the stricter enforcement.
}

\usage{data(traffic)}
\format{
  A data frame with 9 observations on the following 6 variables.
  \describe{
    \item{\code{year}}{a year from 1951 to 1959}
    \item{\code{cn.deaths}}{number of traffic deaths in Connecticut}
    \item{\code{ny}}{deaths per 100,000 in New York}
    \item{\code{cn}}{deaths per 100,000 in Connecticut}
    \item{\code{ma}}{deaths per 100,000 in Massachusetts}
    \item{\code{ri}}{deaths per 100,000 in in Rhode Island}
  }
}
\source{
Tufte, E. R.  \emph{The Visual Display of Quantitative Information}, 2nd ed. Graphics Press, 2001.
}
\references{
Donald T. Campbell and H. Laurence Ross.
"The Connecticut Crackdown on Speeding: Time-Series Data in Quasi-Experimental Analysis",
\emph{Law \& Society Review}
Vol. 3, No. 1 (Aug., 1968), pp. 33-54.

Gene V. Glass.
"Analysis of Data on the Connecticut Speeding Crackdown as a Time-Series Quasi-Experiment"
\emph{Law & Society Review}, Vol. 3, No. 1 (Aug., 1968), pp. 55-76.
}
\examples{
data(traffic)
xyplot(cn.deaths ~ year, traffic,type=c('l','g'))
trafficLong <- reshape(traffic,direction='long', idvar="year", 
				varying=list(3:6), v.names='deathRate',
				times=names(traffic)[3:6],timevar='state')
xyplot(deathRate~year,groups=state,data=trafficLong,type='b',
		auto.key=list(lines=TRUE,points=FALSE,columns=2))
}
\keyword{datasets}
