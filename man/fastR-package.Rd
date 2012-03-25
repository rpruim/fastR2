\name{fastR-package}
\alias{fastR-package}
\alias{fastR}
\docType{package}
\title{Foundations and Applications of Statistics}

\description{
Data sets and utility functions to accompany
\cite{Foundations and Applications of Statistics: An Introduction Using R}
by Randall Pruim.
}
\details{
\tabular{ll}{
Package: \tab fastR\cr
Type: \tab Package\cr
Version: \tab 0.2-9\cr
Date: \tab 2010-12-29\cr
Depends: \tab lattice, grid\cr
Suggests: \tab MASS, faraway, Hmisc, DAAG, multcomp, vcd, car, alr3, corrgram, BradleyTerry 
\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}
}
\author{
Randall Pruim

Maintainer: Randall Pruim <rpruim@calvin.edu>
}
\references{
R. Pruim, 
\cite{Foundations and Applicaitons of Statistics: An Introduction Using R},
AMS, 2011, to appear.
}
\keyword{ package, datasets }
\examples{
require(fastR)
trellis.par.set(theme=col.fastR())
}
