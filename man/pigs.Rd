\name{pigs}
\alias{pigs}
\alias{pigConfig}
\docType{data}
\title{ Pass the Pigs }
\description{
This data set contains information collected from rolling the pair of
pigs (found in the game "Pass the Pigs") 6000 times. 
}
\usage{data(pigs)}
\format{
  A data frame with 6000 observations on the following 6 variables.
  \describe{
    \item{\code{roll}}{roll number (1-6000)}
    \item{\code{black}}{numerical code for position of black pig}
    \item{\code{blackF}}{position of black pig coded as a factor}
    \item{\code{pink}}{numerical code for position of pink pig}
    \item{\code{pinkF}}{position of pink pig coded as a factor}
    \item{\code{score}}{score of the roll}
    \item{\code{height}}{height from which pigs were rolled (5 or 8 inches)}
    \item{\code{start}}{starting position of the pigs 
	(0 = both pigs backwards, 1 = one bacwards one forwards, 2 = both forwards)}
  }
}
\details{

In "Pass the Pigs", players
roll two pig-shaped rubber dice and earn or lose points
depending on the configuration of the rolled pigs. Players compete
individually to earn 100 points.  On each turn, a player rolls he or she decides to
stop or until "pigging out" or 

The pig configurations and their
associated scores are

           1 = Dot Up (0)

           2 = Dot Down (0)

           3 = Trotter (5)

           4 = Razorback (5)

           5 = Snouter (10)

           6 = Leaning Jowler (15)

           7 = Pigs are touching one another (-1; lose all points)

One pig Dot Up and one Dot Down ends the turn (a "pig out") and results in 
0 points for the turn.  
If the pigs touch, the turn is ended and all points for the game must be forfeited.
Two pigs in the Dot Up or Dot Down configuration score 1 point.  
Otherwise,
The scores of the two pigs in different configurations are added together.
The score is doubled if both both pigs have the same configuration, so, for example,
two Snouters are worth 40 rather than 20.

The vector \code{pigConfig} is provided to assist in converting from the numeric 
codes above to pig configurations.
}
\source{
John C. Kern II, Duquesne University (kern@mathcs.duq.edu)
}
\examples{
data(pigs)
table(pigConfig[pigs$black])
}
\keyword{datasets}
