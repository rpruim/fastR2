\name{utilities}
\alias{utilities}
\alias{utilities2}
\docType{data}
\title{ Utilities bills }
\description{
Data from utility bills at a residence.
}
\usage{
data(utilities)

data(utilities2)
}
\format{
  A data frame the following variables.
  \describe{
    \item{\code{month}}{month (coded as a number)}
    \item{\code{day}}{day of month on which bill was calculated}
    \item{\code{year}}{year of bill}
    \item{\code{temp}}{average temperature (F) for billing period}
    \item{\code{kwh}}{electricity usage (kwh)}
    \item{\code{ccf}}{gas usage (ccf)}
    \item{\code{thermsPerDay}}{a numeric vector}
    \item{\code{billingDays}}{number of billing days in billing period}
    \item{\code{totalbill}}{total bill (in dollars)}
    \item{\code{gasbill}}{gas bill (in dollars)}
    \item{\code{elecbill}}{exectric bill (in dollars)}
    \item{\code{notes}}{notes about the billing period}
    \item{\code{ccfpday}}{average gas usage per day [\code{utilities2} only]}
    \item{\code{kwhpday}}{average electric usage per day [\code{utilities2} only]}
    \item{\code{gasbillpday}}{gas bill divided by billing days [\code{utilities2} only]}
    \item{\code{elecbillpday}}{electric bill divided by billing days a numeric vector [\code{utilities2} only]}
    \item{\code{totalbillpday}}{total bill divided by billing days a numeric vector [\code{utilities2} only]}
    \item{\code{therms}}{\code{thermsPerDay * billingDays} [\code{utilities2} only]}
    \item{\code{monthsSinceY2K}}{months since 2000 [\code{utilities2} only]}
  }
}
\source{
Daniel T. Kaplan, \emph{Statistical modeling: A fresh approach}, 2009. 
}
\examples{
data(utilities); data(utilities2)
xyplot(gasbill ~ temp, utilities)
xyplot(gasbillpday ~ temp, utilities2)
}
\keyword{datasets}
