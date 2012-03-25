\name{mathnoise}
\alias{mathnoise}
\docType{data}
\title{ Test performance and noise }
\description{
In this experiment, hyperactive and control students were given a mathematics test in
either a quiet or loud testing environment.
}
\usage{data(mathnoise)}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{score}}{score on a mathematics test}
    \item{\code{noise}}{a factor with levels \code{hi} \code{lo}}
    \item{\code{group}}{a factor with levels \code{control} \code{hyper}}
  }
}
\source{
Sydney S. Zentall and Jandira H. Shaw, Effects of classroom noise on perfor- 
mance and activity of second-grade hyperactive and control children, \emph{Journal 
of Educational Psychology} 72 (1980), no. 6, 830.
}
\examples{
data(mathnoise)
xyplot(score~noise,mathnoise, group=group, type='a', 
		auto.key=list(columns=2,lines=TRUE,points=FALSE))
}
\keyword{datasets}
