ci3 <- function(x, r = 1000, conf.level = 0.95) {
  x.bar <- mean(x); n <- length(x) 
  sqrtn <- sqrt(n); SE <- sd(x) / sqrtn
  Boot <- do(r) * {
    rx <- resample(x)
    mean.boot <- mean(rx)
    se.boot <- sd(rx) / sqrtn
    c(mean.boot = mean.boot,
         t.boot = (mean.boot - x.bar) / se.boot
    )
  }
  q <- cdata(~ t.boot, data = Boot, 0.95)
  tint <- stats::t.test(x) %>% confint()
  data.frame(
    method = c("percentile", "bootstrap-t", "t"),
    estimate = x.bar,
    lo = c(
      quantile(Boot$mean.boot, 0.025),
      x.bar - q[2] * SE,
      tint$lower),
    hi = c(  
      quantile(Boot$mean.boot, 0.975),
      x.bar
      - q[1] * SE,
      tint$upper)
    )
}
# eample use
ci3(rgamma(20, 2, 1))

