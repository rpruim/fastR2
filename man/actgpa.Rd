\name{actgpa}
\alias{actgpa}
\docType{data}
\title{ ACT scores and GPA }
\description{
ACT scores and college GPA for a small sample of college students.
}
\usage{data(actgpa)}
\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{ACT}}{a numeric vector}
    \item{\code{GPA}}{a numeric vector}
  }
}
\examples{
xyplot(GPA ~ ACT, actgpa)
}
\keyword{datasets}
