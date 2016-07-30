eci <- function(x, conf.level = 0.95, type = c("S", "U")) {
  type <- match.arg(type)
  pv <- 
    switch(
      type,
      U = function(x, rate) {pgamma(sum(x) * rate, shape = length(x), rate = 1)},
      S = function(x, rate) {pgamma(sum(x), shape = length(x), rate = rate)}
    )
  alpha <- (1 - conf.level) / 2             
  list(
    type = type,
    estimate = 1 / mean(x),
    conf.int = 
      c(
        uniroot(function(rate) pv(x, rate) - alpha,       c(0.001, 1000))$root,
        uniroot(function(rate) pv(x, rate) - (1 - alpha), c(0.001, 1000))$root
      )
  )
}
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 1/10, method = eci, method.args = list(type = "U"))
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 1/10, method = eci, method.args = list(type = "S"))

