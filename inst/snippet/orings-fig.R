pred_damage <- makeFun(orings.model)
Pred_data <- 
  tibble(
    temp = seq(30, 100, by = 2),
    pred = pred_damage(temp))
gf_point(damage / 6 ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred ~ temp, data = Pred_data) %>%
  gf_lims(x = c(30, 100), y = c(-0.05, 1.05)) %>%
  gf_labs(y = "proportion of O-rings damaged")

orings <- orings %>% mutate(fail = as.numeric(failure))
gf_point(fail ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred ~ temp, data = Pred_data) %>%
  gf_lims(x = c(30, 100), y = c(-0.05, 1.05)) %>%
  gf_labs(y = "O-ring failure (1 = yes, 0 = no)")

