\name{students}
\alias{students}
\docType{data}
\title{ Standardized test scores and GPAs }
\description{
Standardized test scores and GPAs for 1000 students.
}
\usage{data(students)}
\format{
  A data frame with 1000 observations on the following 6 variables.
  \describe{
    \item{\code{ACT}}{ACT score}
    \item{\code{SAT}}{SAT score}
    \item{\code{Grad}}{has the student graduated from college?}
    \item{\code{GradGPA}}{college GPA at graduation}
    \item{\code{HSGPA}}{high school GPA}
    \item{\code{Cohort}}{year of graduation or expected graduation}
  }
}
\examples{
data(students)
xyplot(ACT ~ SAT, students)
xyplot(GradGPA ~ HSGPA, students)
}
\keyword{datasets}
