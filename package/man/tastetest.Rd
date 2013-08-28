\docType{data}
\name{tastetest}
\alias{taste1}
\alias{tastetest}
\title{Taste test data}
\format{A data frame with 16 observations on 2 (\code{taste1}) or 4
(\code{tastetest}) variables.  \itemize{ \item{score}{taste score
from a group of 50 testers} \item{scr}{a factor with levels
\code{coarse} \code{fine}} \item{liq}{a factor with levels \code{hi}
\code{lo}} \item{type}{a factor with levels \code{A} \code{B}
\code{C} \code{D}} }}
\source{
  E. Street and M. G. Carroll, \emph{Preliminary evaluation
  of a food product}, Statistics: A Guide to the Unknown
  (Judith M. Tanur et al., eds.), Holden-Day, 1972, pp.
  220-238.
}
\description{
  Tthe results from a study comparing different preparation
  methods for taste test samples.
}
\details{
  The samples were prepared for tasting using either a
  coarse screen or a fine screen, and with either a high or
  low liquid content. A total taste score is recorded for
  each of 16 groups of 50 testers each. Each group had 25
  men and 25 women, each of whom scored the samples on a
  scale from -3 (terrible) to 3 (excellent).  The sum of
  these individual scores is the overall taste score for
  the group.
}
\examples{
data(tastetest)
data(taste1)
require(Hmisc)
xyplot(score~scr,tastetest)
xyplot(score~scr,groups=liq,tastetest,type='a')
summary(score~scr,tastetest)
}
\keyword{datasets}

