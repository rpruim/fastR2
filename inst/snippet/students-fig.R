act.glm <- glm(grad ~ ACT, data = Students, family = "binomial")
fit1 <- makeFun(act.glm)
gf_point(prop.grad ~ ACT.decile, 
         data = Students %>% 
           mutate(ACT.decile = ntiles(ACT, 10, format = "mean")) %>%
           group_by(ACT.decile) %>% 
           summarise(prop.grad = prop(grad))
) %>% 
  gf_fun(fit1(act) ~ act)
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

