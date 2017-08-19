Sims <-
  (do(400) * ci3(rgamma(20, 1, 1/2))) %>%
  mutate(
    status = c("lo", "good", "hi")[1 + (2 <= lo) + (2 < hi)])

