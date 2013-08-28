\docType{data}
\name{trebuchet}
\alias{trebuchet}
\alias{trebuchet1}
\alias{trebuchet2}
\title{Trebuchet data}
\format{Data frames with the following variables.  \itemize{
\item{object}{the object serving as projectile\code{bean} \code{big
washerb} \code{bigWash} \code{BWB} \code{foose} \code{golf} \code{MWB}
\code{SWB} \code{tennis ball} \code{wood}}
\item{projectileWt}{weight of projectile (in grams)}
\item{counterWt}{weight of counter weight (in kg)}
\item{distance}{distance projectile traveled (in cm)}
\item{form}{a factor with levels \code{a} \code{b} \code{B} \code{c}
describing the configuration of the trebuchet.} }}
\source{
  Data collected by Andrew Pruim as part of a Science
  Olympiad competition.
}
\description{
  Measurements from an experiment that involved firing
  projectiles with a small trebuchet under different
  conditions.
}
\examples{
data(trebuchet); data(trebuchet1); data(trebuchet2)
xyplot(distance~projectileWt, trebuchet1)
xyplot(distance~projectileWt, trebuchet2)
xyplot(distance~projectileWt, groups=projectileWt, trebuchet)
}
\keyword{datasets}

