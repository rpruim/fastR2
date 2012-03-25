\name{wilson.ci}
\alias{wilson.ci}
\alias{wald.ci}
\title{Confidence Intervals for Proportions}
\description{
Alternatives to code{prop.test()} and \code{binom.test()}.
}
\usage{
wilson.ci(x, n = 100, conf.level = 0.95)
wald.ci(x, n = 100, conf.level = 0.95) 
}
\arguments{
  \item{x}{number of 'successes'}
  \item{n}{number of trials}
  \item{conf.level}{confidence level}
}
\details{
\code{wald.ci()} produces Wald confidence intervals.
\code{wilson.ci()} produces Wilson confidence intervals (also called
``plus-4'' confidence intervals) which are Wald intervals computed
from data formed by adding 2 successes and 2 failures.  
The Wilson confidence intervals have better coverage rates 
for small samples.
}
\value{
Lower and upper bounds of a two-sided confidence interval.
}
\references{
A. Agresti and B. A. Coull, Approximate is better then `exact' for interval 
estimation of binomial proportions, \emph{American Statistician} 52 (1998), 119--126. 
}
\author{Randall Pruim}

\examples{
prop.test(12,30)
prop.test(12,30, correct=FALSE)
wald.ci(12,30)
wilson.ci(12,30)
wald.ci(12+2,30+4)
}
