\name{airlineArrival}
\alias{airlineArrival}
\docType{data}
\title{ Airline On-Time Arrival Data }
\description{
Flights categorized by destination city, airline, and whether or not the
flight was on time.
}
\usage{data(airlineArrival)}
\format{
  A data frame with 11000 observations on the following 3 variables.
  \describe{
    \item{\code{Airport}}{a factor with levels \code{LosAngeles} \code{Phoenix} \code{SanDiego} \code{SanFrancisco} \code{Seattle}}
    \item{\code{Result}}{a factor with levels \code{Delayed} \code{OnTime}}
    \item{\code{Airline}}{a factor with levels \code{Alaska} \code{AmericaWest}}
  }
}
\source{
Barnett, Arnold. 1994. ``How numbers can trick you.'' 
\emph{Technology Review}, vol. 97, no. 7, pp. 38--45.
}
\references{
These and similar data appear in many text books under the topic of 
Simpson's paradox.
}

\examples{
row.perc(xtabs(~Airline+Result, airlineArrival))
for (city in levels(airlineArrival$Airport)) {
	cat(paste('\nArriving in ', city,':\n',sep=''))
	print(row.perc(xtabs(~Airline+Result, airlineArrival, 
		subset=Airport==city)))
}
}
\keyword{datasets}
