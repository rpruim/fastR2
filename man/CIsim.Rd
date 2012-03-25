\name{CIsim}
\alias{CIsim}
\title{ Simulated confidence intervals }
\description{
Confidence intervals calculated from simulated data are stored in
a data frame that allows further processing (including plotting).
}
\usage{
CIsim(n, samples = 100, rdist = rnorm, args = list(), 
	estimand = 0, conf.level = 0.95, 
	method = t.test, method.args = list(), 
	interval = function(x) {
    	do.call(method, c(list(x, conf.level = conf.level), method.args))$conf.int
	}, 
	estimate = function(x) {
    	do.call(method, c(list(x, conf.level = conf.level), method.args))$estimate
	}, 
	verbose = TRUE)
}
\arguments{
  \item{n}{sample size }
  \item{samples}{number of samples to simulate}
  \item{rdist}{random distribution function for generating random samples; defaults to \code{rnorm} }
  \item{args}{arguments for \code{rdist}}
  \item{estimand}{ value of estimand }
  \item{conf.level}{level of confidence }
  \item{method}{function for computing a confidence interval and point estimate from data.  
  \code{method} should return a list or structure containing named components \code{conf.int} 
  and \code{estimate} (as in the default \code{t.test}) unless \code{interval} and 
  \code{estimate} are supplied.  
  }
  \item{method.args}{additional arguments for \code{method}.}
  \item{interval}{ a function for computing a confidence interval from data}
  \item{estimate}{ a function for computing the point estimate from data}
  \item{verbose}{ should a report of percentage of simulated intervals that cover the 
  estimand be printed to the screen?}
}
\details{
This function is design for flexibility and ease of use rather than for efficiency.
The goal is to make it easy to estimate coverage rates for a wide range of 
confidence intervals and population distributions.  The resulting data frame 
stores data in a manner suitable for plotting with \code{xYplot()} (see examples).
}
\value{
A data frame with one row per sample and variables 
\code{estimate} (point estimate),
\code{lower} (lower bound of confidence interval),
\code{upper} (upper bound of confidence interval),
\code{cover} (boolean indicating if estimand is in the interval), and 
\code{sample} (a counter, primarily useful for plotting)
}
\author{ Randall Pruim }

\seealso{
\code{xYplot} in \code{Hmisc}.
}
\examples{
CIsim(n=10,100) -> foo
if (require(Hmisc)) {
	xYplot(Cbind(estimate,lower,upper)~sample, foo,groups=cover, col=c('red','navy'),lwd=3,cap=0)
}
}
\keyword{ demo }
\keyword{ stats }% __ONLY ONE__ keyword per line
