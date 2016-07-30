eci2 <- function(x, conf.level = 0.95, type = c("S", "U")) {
  type <- match.arg(type)
  pv <- 
    switch(
      type,
      U = function(x, mu) {pgamma(sum(x) / mu, shape = length(x), rate = 1)},
      S = function(x, mu) {pgamma(sum(x), shape = length(x), rate = 1/mu)}
    )
               
  list(
    type = type,
    estimate = mean(x),
    conf.int = 
      c(
        uniroot(function(mu) pv(x, mu) -     conf.level, c(0.01, 100))$root,
        uniroot(function(mu) pv(x, mu) - 1 + conf.level, c(0.01, 100))$root
      )
  )
}
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 10, method = eci2, method.args = list(type = "U"))
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 10, method = eci2, method.args = list(type = "S"))

