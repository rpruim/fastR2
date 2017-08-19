theta <- 1.8
set.seed(123)
rfoo <- function(n, theta) {runif(n)^(1 / (theta + 1))}
pfoo <- function(q, theta) {
  q^(theta + 1)
}
qfoo <- function(p, theta) {
  p^(1 / (theta + 1))
}

dfoo <- function(x, theta, log = FALSE) {
  if (log) {
    log(theta + 1) + theta * log(x)
  } else {
    (theta + 1) * x^theta
  }
}
x <- round(rfoo(30, theta), 2); x
gf_dhistogram(~ x, binwidth = 0.1) %>%
  gf_dist("foo", theta = 1.8)

