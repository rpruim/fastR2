Plot_data <-
  expand.grid(
    pi = c(0.5, 0.3, 0.1, 0.05),
    n = c(10, 40, 80, 800),
    p = ppoints(50)
  ) %>%
  mutate(
    y = qbinom(p, n, pi),
    x = qnorm(p, n * pi, sqrt(n * pi * (1-pi)))
  ) 

# gf_qq( ~ y, data = Plot_data) %>%
# #  gf_abline(slope = 1, intercept = 0) %>%
#   gf_facet_grid(paste("pi=", pi, sep = "") ~ paste("n=", n, sep = ""), 
#                 scales = "free") %>%
#   gf_labs(
#     y = expression(qbinom(p, n, pi)),
#     x = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi))))
#   ) 
p <- xyplot(
  y ~ x | paste("n=", n, sep = "") * paste("pi=", pi, sep = ""),
  data = Plot_data,
  scales = list(relation = "free"),
  cex = 0.6,
  ylab = expression(qbinom(p, n, pi)),
  xlab = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi)))),
  panel = function(x, y, ...){
    panel.abline(0, 1, ...)
    panel.xyplot(x, y, ...)
  })
latticeExtra::useOuterStrips(p)

