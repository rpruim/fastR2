\name{concrete}
\alias{concreteAll}
\alias{concrete28}
\docType{data}
\title{Concrete Compressive Strength Data}
\description{
These data were collected by I-Cheng Yeh to determine how the 
compressive strength of concrete is related to its ingredients
(cement, blast furnace slag, fly ash, water, superplasticizer, 
coarse aggregate, and fine aggregate) and age.
}
\usage{data(concreteAll); data(concrete28)
}
\format{
  \code{concreteAll} is a  data frame with the following 9 variables.
  \describe{
    \item{\code{cement}}{amount of cement (kg/m^3)}
    \item{\code{slag}}{amount of blast furnace slag (kg/m^3)}
    \item{\code{ash}}{amount of fly ash(kg/m^3)}
    \item{\code{water}}{amount of water (kg/m^3)}
    \item{\code{superP}}{amount of superplasticizer (kg/m^3)}
    \item{\code{coarseAg}}{amount of coarse aggregate (kg/m^3)}
    \item{\code{fineAg}}{amount of fine aggregate (kg/m^3)}
    \item{\code{age}}{age of concrete in days }
    \item{\code{strength}}{compressive strength measured in MPa}
  }
  \code{concrete28} is a subset of \code{concreteAll}.
}
\source{
Data were obtained from the Machine Learning Repository 
(\url{http://archive.ics.uci.edu/ml/})
where they were deposited by I-Cheng Yeh (icyeh@chu.edu.tw)
who retains the copyright for these data.
}
\references{
I-Cheng Yeh (1998), 
"Modeling of strength of high performance concrete using artificial
neural networks," 
\cite{Cement and Concrete Research}, Vol. 28, No. 12, pp. 1797-1808.
}
\examples{
data(concreteAll)
data(concrete28)
}
\keyword{datasets}
