\name{miaa05}
\alias{miaa05}
\docType{data}
\title{
MIAA basketball 2004-2005 season
}
\description{
Individual player statistics for the 2004-2005 Michigan Intercollegiate Athletic Association
basketball season.
}
\usage{data(miaa05)}
\format{
  A data frame with 134 observations on the following 27 variables.
  \describe{
    \item{\code{Number}}{jersey number}
    \item{\code{Player}}{player's name}
    \item{\code{GP}}{games played}
    \item{\code{GS}}{games started}
    \item{\code{Min}}{minutes played}
    \item{\code{AvgMin}}{average minutes played per game}
    \item{\code{FG}}{field goals made}
    \item{\code{FGA}}{field goals attempted}
    \item{\code{FGPct}}{field goal percentage}
    \item{\code{FG3}}{3-point field goals made}
    \item{\code{FG3A}}{3-point field goals attempted}
    \item{\code{FG3Pct}}{3-point field goal percentage} 
    \item{\code{FT}}{free throws made}
    \item{\code{FTA}}{free throws attempted}
    \item{\code{FTPct}}{free throw percentage}
    \item{\code{Off}}{offensive rebounds}
    \item{\code{Def}}{defensive rebounds}
    \item{\code{Tot}}{total rebounds }
    \item{\code{RBG}}{rebounds per game}
    \item{\code{PF}}{personal fouls}
    \item{\code{FO}}{games fouled out}
    \item{\code{A}}{assists}
    \item{\code{TO}}{turn overs}
    \item{\code{Blk}}{blocked shots}
    \item{\code{Stl}}{steals}
    \item{\code{Pts}}{points scored}
    \item{\code{PTSG}}{points per game}
  }
}
\source{ MIAA sports archives (\url{http://www.miaa.org/})
}
\examples{
data(miaa05)
histogram(~FTPct,miaa05)
}
\keyword{datasets}
