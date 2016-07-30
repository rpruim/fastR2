Sims <-
  expand.grid(
  n = c(35, 100), 
  pi = c(0.2, 0.3, 0.4, 0.5),
  method = c("Wald", "Wilson", "score"), 
  rep = 1:2000) %>%
  group_by(n, pi, method, rep) %>% 
  do(confint(binom.test(rbinom(1, .$n, .$pi), n = .$n, ci.method = .$method)))

Sims %>%
  group_by(n, pi, method) %>% 
  summarise(cover = prop(lower <= pi & pi <= upper))

