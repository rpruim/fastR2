\name{endurance}
\alias{endurance}
\docType{data}
\title{ Endurance and vitamin C }
\description{
The effect of a single 600 mg dose of ascorbic acid versus a sugar placebo on
the muscular endurance (as measured by repetitive grip strength trials) of
fifteen male volunteers (19-23 years old).
}
\usage{data(endurance)}
\format{
  A data frame with 15 observations on the following 5 variables.
  \describe{
    \item{\code{Vitamin}}{number of repetitions until reaching 50\% maximal grip 
	after taking viatimin}
    \item{\code{First}}{which treatment was done first, 
	a factor with levels \code{Placebo} \code{Vitamin}}
    \item{\code{Placebo}}{number of repetitions until reaching 50\% maximal grip strength 
	after taking placebo}
  }
}
\details{
Three initial maximal contractions were performed for each subject, with the
greatest value indicating maximal grip strength. Muscular endurance was
measured by having the subjects squeeze the dynamometer, hold the contraction
for three seconds, and repeat continuously until a value of 50\% maximum grip
strength was achieved for three consecutive contractions. Endurance was
defined as the number of repetitions required to go from maximum grip strength
to the initial 50\% value. Subjects were given frequent positive verbal
encouragement in an effort to have them complete as many repetitions as
possible.  

The study was conducted in a double-blind manner with crossover.
}
\source{
These data are available from OzDASL, the Australasian data and story library
(\url{http://www.statsci.org/data/}).
}
\references{
Keith, R. E., and Merrill, E. (1983). 
The effects of vitamin C on maximum grip strength and muscular endurance. 
\emph{Journal of Sports Medicine and Physical Fitness}, 23, 253-256.  
}
\examples{
data(endurance)
t.test(endurance$Vitamin, endurance$Placebo, paired=TRUE)
t.test(log(endurance$Vitamin), log(endurance$Placebo), paired=TRUE)
t.test(1/endurance$Vitamin, 1/endurance$Placebo, paired=TRUE)
xqqmath( ~ Vitamin - Placebo, endurance)
xqqmath( ~ log(Vitamin) - log(Placebo), endurance)
xqqmath( ~ 1/Vitamin - 1/Placebo, endurance)
}
\keyword{datasets}
