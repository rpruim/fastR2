D <- data_frame(
  x1 = runif(100, 0, 10),
  x2 = runif(100, 0, 10),
  y1 = 5 + 2 * x1 + 3 * x2 + rnorm(100, sd = 4),
  y2 = 5 + 2 * x1 + 3 * x2 - x1 * x2 + rnorm(100, sd = 4)
)
lm(y1 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "additive model; y1")
lm(y1 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y1")
lm(y2 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "additive model; y2")
lm(y2 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y2")

