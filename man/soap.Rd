
\name{soap}
\alias{soap}
\docType{data}
\title{ Dwindling soap }
\description{
A bar of soap was weighed after showering to see how much soap was used each shower.
}
\usage{data(soap)}
\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{Date}}{}
    \item{\code{Day}}{days since start of soap usage and data collection}
    \item{\code{Weight}}{weight of bar of soap (in grams) }
  }
}
\details{
According to Rex Boggs:  

I had a hypothesis that the daily weight of my bar
of soap [in grams] in my shower wasn't a linear function, the reason being that
the tiny little bar of soap at the end of its life seemed to hang around for
just about ever. I wanted to throw it out, but I felt I shouldn't do so until
it became unusable. And that seemed to take weeks.

	Also I had recently bought some digital kitchen scales and felt I needed to
	use them to justify the cost. I hypothesized that the daily weight of a bar
	of soap might be dependent upon surface area, and hence would be a
	quadratic function \dots .

The data ends at day 22. On day 23 the soap broke into two pieces and one piece
went down the plughole. 
}
\source{
Data collected by Rex Boggs and available from OzDASL (\url{http://www.statsci.org/data/index.html}).
}
\examples{
data(soap)
xyplot(Weight~Day,soap)
}
\keyword{datasets}
