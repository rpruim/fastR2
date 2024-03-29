% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snippet.R
\name{snippet}
\alias{snippet}
\title{Display or execute a snippet of R code}
\usage{
snippet(
  name,
  eval = TRUE,
  execute = eval,
  view = !execute,
  echo = TRUE,
  ask = getOption("demo.ask"),
  verbose = getOption("verbose"),
  lib.loc = NULL,
  character.only = FALSE,
  regex = NULL,
  max.files = 10L
)
}
\arguments{
\item{name}{name of snippet}

\item{eval}{a logical.  An alias for `execute`.}

\item{execute}{a logical.  If \code{TRUE}, snippet code is executed.  (The
code and the results of the execution will be visible if \code{echo} is
\code{TRUE}.)}

\item{view}{a logical. If \code{TRUE}, snippet code is displayed 'as is'.}

\item{echo}{a logical. If \code{TRUE}, show the R input when executing.}

\item{ask}{a logical (or "default") indicating if
\code{devAskNewPage(ask=TRUE)} should be called before graphical output
happens from the snippet code. The value "default" (the factory-fresh
default) means to ask if \code{echo == TRUE} and the graphics device appears
to be interactive. This parameter applies both to any currently opened
device and to any devices opened by the demo code. If this is evaluated to
\code{TRUE} and the session is interactive, the user is asked to press
RETURN to start.}

\item{verbose}{a logical. If \code{TRUE}, additional diagnostics are
printed.}

\item{lib.loc}{character vector of directory names of R libraries, or NULL.
The default value of NULL corresponds to all libraries currently known.}

\item{character.only}{logical. If \code{TRUE}, use \code{name}as character
string.}

\item{regex}{ignored.  Retained for backwards compatibility.}

\item{max.files}{an integer limiting the number of files retrieved.}
}
\description{
This command will display and/or execute small snippets of R code from the
book \emph{Foundations and Applications of Statistics: An Introduction Using
R}.
}
\details{
\code{snippet} works much like \code{\link{demo}}, but the interface is
simplified. Partial matching is used to select snippets, so any unique
prefix is sufficient to specify a snippet.  Sequenced snippets (identified by
trailing 2-digit numbers) will be executed in sequence if a unique prefix to
the non-numeric portion is given.  To run just one of a sequence of snippets,
provide the full snippet name.  See the examples.
}
\examples{
snippet("normal01")
# prefix works
snippet("normal")
# this prefix is ambiguous
snippet("norm")
# sequence of "histogram" snippets
snippet("hist", eval = FALSE, echo = TRUE, view = FALSE)
# just one of the "histogram" snippets
snippet("histogram04", eval = FALSE, echo = TRUE, view = FALSE)
# Prefix too short, but a helpful message is displayed
snippet("h", eval = FALSE, echo = TRUE, view = FALSE)
}
\seealso{
\code{\link{demo}}, \code{\link{source}}.
}
\author{
Randall Pruim
}
\keyword{utilities}
