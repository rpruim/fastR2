\name{paperplanes}
\alias{paperplanes}
\docType{data}
\title{
Paper airplanes
}
\description{
Student-collected data from an experiment investigating the design of 
paper airplanes.
}
\usage{data(paperplanes)}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{distance}}{distance plane traveled (cm)}
    \item{\code{paper}}{type of paper used}
    \item{\code{angle}}{a numeric vector}
    \item{\code{design}}{design of plane (\code{hi performance} or \code{simple})}
    \item{\code{order}}{order in which planes were thrown}
  }
}
\details{
These data were collected by Stewart Fischer and David Tippetts, statistics
students at the Queensland University of Technology in a subject taught by Dr.
Margaret Mackisack. Here is their description of the data and its collection:

The experiment decided upon was to see if by using two different designs
of paper aeroplane, how far the plane would travel. In considering this, the
question arose, whether different types of paper and different angles of
release would have any effect on the distance travelled. Knowing that paper
aeroplanes are greatly influenced by wind, we had to find a way to eliminate
this factor. We decided to perform the experiment in a hallway of the
University, where the effects of wind can be controlled to some extent by
closing doors.

In order to make the experimental units as homogeneous as possible we
allocated one person to a task, so person 1 folded and threw all planes,
person 2 calculated the random order assignment, measured all the distances,
checked that the angles of flight were right, and checked that the plane
release was the same each time.

The factors that we considered each had two levels as follows:

    Paper: A4 size, 80g and 50g

    Design: High Performance Dual Glider, and Incredibly Simple Glider (patterns attached to original report)

    Angle of release: Horizontal, or 45 degrees upward.

    The random order assignment was calculated using the random number
function of a calculator. Each combination of factors was assigned a number
from one to eight, the random numbers were generated and accordingly the order
of the experiment was found.
}
\source{
These data are also available at OzDASL, the Australasian Data and Story Library 
(\url{http://www.statsci.org/data/}).
}
\references{
Mackisack, M. S. (1994). What is the use of experiments conducted by
statistics students? \emph{Journal of Statistics Education}, 2, no 1.
}
\examples{
data(paperplanes)
}
\keyword{datasets}
