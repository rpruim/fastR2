pow <- function(effect) {
    power.t.test(delta = effect, n = 50)$power
}
effect = seq(0, 2, by = 0.05)
gf_line(pow(effect) ~ effect) %>%
  gf_labs(y = "power", x = "effect size", 
          title = "Power of a 2-sample test (n = 50)")

