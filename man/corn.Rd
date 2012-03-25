\name{corn}
\alias{corn}
\docType{data}
\title{ Corn Yield }
\description{
William Gosset analyzed data from an experiment
comparing the yield of regular and kiln-dried corn.
}
\usage{data(corn)}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{reg}}{yield of regular corn (lbs/acre)}
    \item{\code{kiln}}{yield of kiln-dried corn (lbs/acre)}
  }
}
\details{
Gosset (Student) reported on the results of seeding plots with two different
kinds of seed. Each type of seed (regular and kiln-dried) was planted in adjacent
plots, accounting for 11 pairs of "split" plots. 
}

\source{
These data are also available at DASL, the data and story library
(\url{http://lib.stat.cmu.edu/DASL/}).
}
\references{
W.S. Gosset, "The Probable Error of a Mean," Biometrika, 6 (1908), pp 1-25.
}
\examples{
corn2 <- stack(corn)
names(corn2) <- c('yield','treatment')
lm(yield ~ treatment, corn2)
t.test(yield ~ treatment, corn2)
t.test(corn$reg,corn$kiln)
}
\keyword{datasets}
