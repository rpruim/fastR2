sat.glm <- glm(grad ~ SAT, data = Students, family = "binomial")
fit2 <- makeFun(sat.glm)
gf_point(
  prop.grad ~ SAT.decile,
  data = Students %>% 
    mutate(SAT.decile = ntiles(SAT, 10, format = "mean")) %>%
    group_by(SAT.decile) %>% 
    summarise(prop.grad = prop(grad))
) %>%
  gf_fun(fit2(sat) ~ sat)

