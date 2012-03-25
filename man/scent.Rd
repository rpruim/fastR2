\name{scent}
\alias{scent}
\docType{data}
\title{ Maze tracing and scents }
\description{
Subjects were asked to to complete a pencil and paper
maze when they were smelling a floral scent and when they were not.
}
\usage{data(scent)}
\format{
  A data frame with 21 observations on the following 12 variables.
  \describe{
    \item{\code{id}}{ID number}
    \item{\code{sex}}{a factor with levels \code{F} and\code{M}}
    \item{\code{smoker}}{a factor with levels \code{N}, \code{Y}}
    \item{\code{opinion}}{opinion of the odor (\code{indiff}, \code{neg}, or \code{pos}})
    \item{\code{age}}{age of subject (in years)}
    \item{\code{first}}{which treatment was first, \code{scented} or \code{unscented}}
    \item{\code{u1}}{time (in seconds) in first unscented trial}
    \item{\code{u2}}{time (in seconds) in second unscented trial}
    \item{\code{u3}}{time (in seconds) in third unscented trial}
    \item{\code{s1}}{time (in seconds) in first scented trial}
    \item{\code{s2}}{time (in seconds) in second scented trial}
    \item{\code{s3}}{time (in seconds) in third scented trial}
  }
}

\source{These data are also available at DASL, the data and story library (\url{http://lib.stat.cmu.edu/DASL/}).
}
\references{
Hirsch, A. R., and Johnston, L. H. "Odors and Learning," 
Smell \& Taste Treatment and Research Foundation, Chicago.
}
\examples{
data(scent)
summary(scent)
}
\keyword{datasets}
