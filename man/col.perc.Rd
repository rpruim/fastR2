\name{col.perc}
\alias{col.perc}
\alias{row.perc}
\title{ Row and Column Percentages }
\description{
Convenience wrappers around \code{\link{apply}()} to compute
row and column percentages of matrix-like structures, including
output of \code{\link{xtabs}}.  
}
\usage{
col.perc(x)
row.perc(x)
}
\arguments{
  \item{x}{ matrix-like structure }
}
\author{
Randall Pruim
}

\examples{
row.perc(xtabs(~Airline+Result,airlineArrival))
}
\keyword{ array }
