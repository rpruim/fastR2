settings <- expand.grid( kernel = c("gaussian", "triangular",
                                  "rectangular", "epanechinikov"),
                         size = c(10, 30, 100),
                         adjust = c(1/3, 1, 3)
)
results <- 
  bind_rows(
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "normal", 
        mise = mise(size = size, reps = 500)),
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "exp", 
        mise = mise(size = size, reps = 500, dist = "exp")),
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "beta", 
        mise = mise(size = size, reps = 500, dist = "beta", 
                    args = list(shape1 = 0.5, shape2 = 0.5)))
    )

