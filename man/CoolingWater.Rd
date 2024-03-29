% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{CoolingWater}
\alias{CoolingWater}
\alias{CoolingWater1}
\alias{CoolingWater2}
\alias{CoolingWater3}
\alias{CoolingWater4}
\title{Cooling Water}
\format{
A data frame with the following variables.
  \describe{
      \item{\code{time}}{ time in seconds}
      \item{\code{temp}}{ temperature in Celsius (\code{CoolingWater1}, \code{CoolingWater2}) 
      or Fahrenheit (\code{CoolingWater3}, \code{CoolingWater4})
      }
  }
}
\source{
These data were collected by Stan Wagon and his students at Macelester
College to explore Newton's Law of Cooling and the ways that the law 
fails to capture all of the physics involved in cooling water.
\code{CoolingWater1} and \code{CoolingWater2} appeared in a plot in Wagon (2013) 
and were (approximatley) extracted from the plot.
\code{CoolingWater3} and \code{CoolingWater4} appeared in a plot in Wagon (2005).
The data in 
\code{CoolingWater2} and \code{CoolingWater4} were collected with a film of oil on
the surface of the water to minimize evaporation.
}
\usage{
data(CoolingWater1)

data(CoolingWater2)

data(CoolingWater3)

data(CoolingWater4)
}
\description{
Temperature of a mug of water as it cools.
}
\examples{
data(CoolingWater1)
data(CoolingWater2)
data(CoolingWater3)
data(CoolingWater4)
if (require(ggformula)) {
  gf_line(
    temp ~ time, color = ~ condition, 
    data = rbind(CoolingWater1, CoolingWater2))
}
if (require(ggformula)) {
  gf_line(
    temp ~ time, color = ~ condition, 
    data = rbind(CoolingWater3, CoolingWater4))
}

}
\references{
\itemize{
\item
R. Portmann and S. Wagon. "How quickly does hot water cool?" 
\emph{Mathematica in Education and Research}, 
10(3):1-9, July 2005.
\item
R. Israel, P. Saltzman, and S. Wagon. 
"Cooling coffee without solving differential equations". 
\emph{Mathematics Magazine},  86(3):204-210, 2013.
}
}
\keyword{datasets}
