\name{kids}
\alias{kids}
\docType{data}
\title{ Goals and popularity factors for school kids }
\description{
Subjects were students in grades 4-6 from three school districts in Michigan.
Students were selected from urban, suburban, and rural school districts with
approximately 1/3 of their sample coming from each district. Students indicated
whether good grades, athletic ability, or popularity was most important to
them.  They also ranked four factors: grades, sports, looks, and money, in
order of their importance for popularity.  The questionnaire also asked for
gender, grade level, and other demographic information.
}
\usage{data(kids)}
\format{
  A data frame with 478 observations on the following 11 variables.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{boy} \code{girl}}
    \item{\code{Grade}}{grade in school}
    \item{\code{Age}}{student age}
    \item{\code{Race}}{a factor with levels \code{Other} \code{White}}
    \item{\code{Urban.Rural}}{a factor with levels \code{Rural} \code{Suburban} \code{Urban}}
    \item{\code{School}}{a factor with levels \code{Brentwood Elementary} \code{Brentwood Middle} \code{Brown Middle} \code{Elm} \code{Main} \code{Portage} \code{Ridge} \code{Sand} \code{Westdale Middle}}
    \item{\code{Goals}}{a factor with levels \code{Grades} \code{Popular} \code{Sports}}
    \item{\code{Grades}}{rank of `make good grades' (1=most important for popularity; 4=least important)}
    \item{\code{Sports}}{rank of `beging good at sports' (1=most important for popularity; 4=least important)}
    \item{\code{Looks}}{rank of `beging handsome or pretty' (1=most important for popularity; 4=least important)}
    \item{\code{Money}}{rank of `having lots of money' (1=most important for popularity; 4=least important)}
  }
}
\source{
These data are available at DASL, the data and story library 
(\url{http://lib.stat.cmu.edu/DASL/}).
}
\references{
Chase, M. A., and Dummer, G. M. (1992), "The Role of Sports as a Social
Determinant for Children," Research Quarterly for Exercise and Sport, 63,
418-424.
}
\examples{
data(kids)
xtabs(~Goals + Urban.Rural, kids)
chisq.test(xtabs(~Goals + Urban.Rural, kids))
}
\keyword{datasets}
