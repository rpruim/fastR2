require(ggplot2)
mise <- function(size = 20, reps = 100, dist = "norm", args = list(), ...) {
  results <- do(reps) * {
    data <- do.call(paste0("r", dist), c(list(n = size), args))
    distr <- function(x) { do.call(paste0("d", dist), c(list(x), args)) }
    d <- density(data, ...)
    data.frame(ise = ise(d, distr))
  }
  return( c(mise = mean(~ise, data = results) ) )
}

