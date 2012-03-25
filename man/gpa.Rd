\name{gpa}
\alias{gpa}
\docType{data}
\title{ GPA, ACT, and SAT scores }
\description{
GPA, ACT, and SAT scores for a sample of students.
}
\usage{data(gpa)}
\format{
  A data frame with 271 observations on the following 4 variables.
  \describe{
    \item{\code{satm}}{SAT mathematics score}
    \item{\code{satv}}{SAT verbal score}
    \item{\code{act}}{ACT score}
    \item{\code{gpa}}{college grade point average}
  }
}
\examples{
data(gpa)
splom(gpa)
}
\keyword{datasets}
