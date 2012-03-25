\name{golfballs}
\alias{golfballs}
\docType{data}
\title{ Golf ball numbers }
\description{
Allan Rossman used to live on a golf course in a spot where dozens of balls
would come into his yard every week.  He collected the balls and eventually
tallied up the numbers on the first 5000 golf balls he collected.
Of these 486 bore the number 1, 2, 3, or 4.  The remaining 14 golf balls
were omitted from the data.
}
\usage{data(golfballs)}
\format{
  The format is:
 num [1:4] 137 138 107 104
}
\source{Data collected by Allan Rossman in Carlisle, PA. }
\examples{
data(golfballs)
golfballs/sum(golfballs)
chisq.test(golfballs, p=rep(.25,4))
}
\keyword{datasets}
