\name{littleSurvey}
\alias{littleSurvey}
\docType{data}
\title{
Results from a little survey
}
\description{
These data are from a little survey given to a number 
of students in introductory statistics courses.
Several of the items were prepared in multiple versions and 
distributed randomly to the students.
}
\usage{data(littleSurvey)}
\format{
  A data frame with 279 observations on the following 20 variables.
  \describe{
    \item{\code{number}}{a number between 1 and 30}
    \item{\code{colorVer}}{which version of the 'favorite color' question
	was on the survey. A factor with levels \code{v1} \code{v2}}
    \item{\code{color}}{favorite color if among predefined 
	choices.  A factor with levels 
	\code{} \code{black} \code{green} \code{other} \code{purple} \code{red}}
    \item{\code{otherColor}}{favorite color if not among choices above.}
    \item{\code{animalVer}}{which version of the 'favorite color' question
	was on the survey. A factor with levels \code{v1} \code{v2}}
    \item{\code{animal}}{favorite animal if among predefined choices.
	A factor with levels 
	\code{} \code{elephant} \code{giraffe} \code{lion} \code{other}.}
    \item{\code{otherAnimal}}{favorite animal if not among 
	the predefined choices.}
    \item{\code{pulseVer}}{which version of the 'pulse' question
	was on the survey}
    \item{\code{pulse}}{self-reported pulse}
    \item{\code{TVver}}{which of three versions of the TV question
	was on the survey}
    \item{\code{tvBox}}{a factor with levels \code{<1} \code{>4} \code{>8} \code{1-2} \code{2-4} \code{4-8} \code{none} \code{other}}
    \item{\code{tvHours}}{a numeric vector}
    \item{\code{surpriseVer}}{which of two versions of the 'surprise' question was on the survey}
    \item{\code{surprise}}{a factor with levels \code{no} \code{yes}}
    \item{\code{playVer}}{which of two versions of the 'play' question was on the survey}
    \item{\code{play}}{a factor with levels \code{no} \code{yes}}
    \item{\code{diseaseVer}}{which of two versions of the 'play' question was on the survey}
    \item{\code{disease}}{a factor with levels \code{A} \code{B}}
    \item{\code{homeworkVer}}{which of two versions of the 'homework' question was on the survey}
    \item{\code{homework}}{a factor with levels \code{A} \code{B}}
  }
}
\examples{
data(littleSurvey)
xtabs(~surprise+surpriseVer,littleSurvey)
xtabs(~disease+diseaseVer,littleSurvey)
}
\keyword{datasets}
