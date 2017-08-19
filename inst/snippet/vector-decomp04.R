vdecomp <- function(x, ...) {
  v <- list(...)
  projection <- project(x, v[[1]])
  coefs <-project(x, v[[1]], type = "coef")
  for (i in 2:length(v)) {
    decomp <- vdecomp2(x, projection, v[[i]])
    coefs <- c(coefs * decomp$coefficients[1], decomp$coefficients[2])
    projection <- decomp$projection
  }
  list(coefficients = coefs, projection = projection, 
       remainder = x - projection)
}

