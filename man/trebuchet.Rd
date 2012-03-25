\name{trebuchet}
\alias{trebuchet}
\alias{trebuchet1}
\alias{trebuchet2}
\docType{data}
\title{ Trebuchet data }
\description{
Measurements from an experiment that involved firing projectiles with a small
trebuchet under different conditions.
}
\usage{
data(trebuchet)
data(trebuchet1)
data(trebuchet2)
}
\format{
  Data frames with the following variables.
  \describe{
    \item{\code{object}}{the object serving as projectile\code{bean} \code{big washerb} \code{bigWash} \code{BWB} \code{foose} \code{golf} \code{MWB} \code{SWB} \code{tennis ball} \code{wood}}
    \item{\code{projectileWt}}{weight of projectile (in grams)}
    \item{\code{counterWt}}{weight of counter weight (in kg)}
    \item{\code{distance}}{distance projectile traveled (in cm)}
    \item{\code{form}}{a factor with levels \code{a} \code{b} \code{B} \code{c}
		describing the configuration of the trebuchet.}
  }
}
\source{
Data collected by Andrew Pruim as part of a Science Olympiad competition.
}
\examples{
data(trebuchet); data(trebuchet1); data(trebuchet2)
xyplot(distance~projectileWt, trebuchet1)
xyplot(distance~projectileWt, trebuchet2)
xyplot(distance~projectileWt, groups=projectileWt, trebuchet)
}
\keyword{datasets}
