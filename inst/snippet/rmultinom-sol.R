rmultinom2 <- function(n, size, prob) {
	prob <- prob / sum(prob)
	x <- runif(n * size, 0, 1)
	y <- as.numeric(cut(x, c(0, cumsum(prob))))
  y <- factor(y, levels = 1:length(prob))    # so we get 0 counts recorded
  M <- matrix(y, nrow = n)                     # put results into a matrix
  apply(M, 1, table)                         # create table for each row
}
rmultinom2(4, 100, c(.1, .2, .3, .4))

