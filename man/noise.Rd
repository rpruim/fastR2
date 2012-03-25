\name{noise}
\alias{noise}
\docType{data}
%Test performance in different noise conditions 
\title{ Noise -- unfinished documentation }
\description{
In order to test the effect of room noise, subjects were given a test under 5 
diff sets of conditions: 1) no noise, 2) intermittent low volume, 3) intermittent 
high volume, 4) continuous low volume, and 5) continuous high volume. 
}
\usage{data(noise)}
\format{
  A data frame with 50 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{subject identifier}
    \item{\code{score}}{score on the test}
    \item{\code{condition}}{numeric code for condition}
    \item{\code{volume}}{a factor with levels \code{high} \code{low} \code{none}}
    \item{\code{frequency}}{a factor with levels \code{continuous} \code{intermittent} \code{none}}
  }
}
\examples{
data(noise)
noise2 <- noise[noise$volume != 'none',] 
model <- lm(score~volume*frequency,noise2) 
anova(model)
xyplot(score~volume,noise2, groups=frequency, type='a',
		auto.key=list(columns=2, points=FALSE, lines=TRUE))
}
\keyword{datasets}
