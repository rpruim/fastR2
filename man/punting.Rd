\name{punting}
\alias{punting}
\docType{data}
\title{ American football punting }
\description{
Investigators studied physical characteristics and ability in 13 football
punters. Each volunteer punted a football ten times. The investigators recorded
the average distance for the ten punts, in feet. They also recorded the average
hang time (time the ball is in the air before the receiver catches it), and  
a number of measures of leg strength and flexibility.
}
\usage{data(punting)}
\format{
  A data frame with 13 observations on the following 7 variables.
  \describe{
    \item{\code{distance}}{mean distance for 10 punts (feet) }
    \item{\code{hang}}{mean hang time (seconds) }
    \item{\code{rStrength}}{right leg strength (pounds)}
    \item{\code{lStrength}}{left leg strength (pounds)}
    \item{\code{rFlexibility}}{right leg flexibility (degrees)}
    \item{\code{lFlexibility}}{left leg flexibility (degrees)}
    \item{\code{oStrength}}{overall leg strength (foot-pounds)}
  }
}
\source{
These data are also available at OzDASL (\url{http://www.statsci.org}).
}
\references{
"The relationship between selected physical performance variables and
football punting ability" by the Department of Health, Physical Education and
Recreation at the Virginia Polytechnic Institute and State University, 1983.
}
\examples{
data(punting)
xyplot(hang ~ distance, punting)
}
\keyword{datasets}
