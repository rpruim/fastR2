\docType{data}
\name{fumbles}
\alias{fumbles}
\title{NCAA football fumbles}
\format{A data frame with 120 observations on the following 7 variables.
\itemize{ \item{team}{NCAA football team} \item{rank}{rank
based on fumbles per game through games on November 26, 2010}
\item{W}{number of wins through games on November 26, 2010}
\item{L}{number of losses through games on November 26, 2010}
\item{week1}{number of fumbles on November 6, 2010}
\item{week2}{number of fumbles on November 13, 2010}
\item{week3}{number of fumbles on November 20, 2010} }}
\source{
  \url{http://www.teamrankings.com/college-football/stat/fumbles-per-game}
}
\description{
  This data frame gives the number of fumbles by each NCAA
  FBS team for the first three weeks in November, 2010.
}
\details{
  The fumble counts listed here are total fumbles, not
  fumbles lost.  Some of these fumbles were recovered by
  the team that fumbled.
}
\examples{
data(fumbles)
m <- max(fumbles$week1)
table(factor(fumbles$week1,levels=0:m))
favstats(fumbles$week1)
# compare with Poisson distribution
signif( cbind(
		  fumbles=0:m,
		  observedCount=table(factor(fumbles$week1,levels=0:m)),
		  modelCount= 120* dpois(0:m,mean(fumbles$week1)),
		  observedPct=table(factor(fumbles$week1,levels=0:m))/120,
		  modelPct= dpois(0:m,mean(fumbles$week1))
	) ,3)
showFumbles <- function(x,lambda=mean(x),...) {
	mx <- max(x)
    result <- histogram(~x,type="density", xlim=c(-.5,(mx+2.5)),
		xlab='number of fumbles',
        panel=function(x,y,...){
            panel.histogram(x,alpha=0.8,breaks=seq(-0.5,(mx+2.5),by=1,...))
            panel.points(0:(mx+2),dpois(0:(mx+2),lambda),pch=19,alpha=0.8)
        }
    )
    print(result)
    return(result)
}
showFumbles(fumbles$week1)
showFumbles(fumbles$week2)
showFumbles(fumbles$week3)
}
\keyword{datasets}

