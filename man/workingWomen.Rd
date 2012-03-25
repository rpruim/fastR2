\name{workingWomen}
\alias{workingWomen}
\docType{data}
\title{ Women in the workforce }
\description{
The labor force participation rate of women in each of 19 U.S. cities in each of two years.
# Reference: United States Department of Labor Statistics
# 
# Authorization: free use
# 
# Description: 
# 
# Variable Names:
# 
#    1. City: City in the United States
#    2. labor72: Labor Force Participation rate of women in 1972
#    3. labor68: Labor Force Participation rate of women in 1968 
# 
# The Data:
# 

%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(workingWomen)}
\format{
  A data frame with 19 observations on the following 3 variables.
  \describe{
    \item{\code{city}}{name of a U.S. city (coded as a factor with 19 levels)}
    \item{\code{labor72}}{percent of women in labor force in 1972}
    \item{\code{labor68}}{percent of women in labor force in 1968}
  }
}
\source{
These data are from the United States Department of Labor Statistics and 
are also available at DASL, the Data and Story Library (\url{http://lib.stat.cmu.edu/DASL/}).
}
\examples{
data(workingWomen)
xyplot(labor72 ~ labor68, workingWomen)
}
\keyword{datasets}
