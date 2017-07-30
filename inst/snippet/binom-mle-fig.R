llik <- function (p, t = 14) {
    14*log(p) + 26 * log(1-p)
}
xpts <- seq(0, 1, by = 0.005)
gf_line(llik(xpts) ~ xpts) %>%
  gf_labs(x = expression(pi), y = "log-likelihood")

