# Here is a simple way to estimate ise
ise <- function(density, distr, ...) {
  x <- density$x
  y <- density$y
  diffs <- diff(c(min(x), x, max(x)))
  dx <- .5 * (head(diffs, -1) + tail(diffs, -1))
  sum((y - distr(x, ...))^2 * dx)
}

# some sanity checks
x <- rnorm(100)
d <- density(x)
ise( d, dnorm )

