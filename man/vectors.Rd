\name{vectors}
\alias{dot}
\alias{vlength}
\alias{project}

\title{ Vector functions }
\description{
These functions simplify working with vectors.
}

\usage{
dot(x, y)

vlength(x)

project(x, u = rep(1, length(x)), type = c("vector", "length"))

}

\arguments{
  \item{u}{a numeric vector} 
  \item{x}{a numeric vector} 
  \item{y}{a numeric vector} 
  \item{type}{one of \code{c("vector","length")} indicating whether 
  the projection should be returned as a vector (the default)
  or a length.}

}

\value{
\code{dot} returns the dot product of two vectors.

\code{vlength} returns the length of a vector.

\code{project(x,u)} returns the vector (or its length)
that results from projecting \code{x} in the direction of \code{u}.
It is not necessary that \code{u} be a unit vector.

}

\author{
Randall Pruim
}


\examples{
v <- c(1,1,1,1)
x <- c(1,2,3,4)
vlength(v)
dot(x,v)
dot(x,v) / vlength(x)^2
project(x,v)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
