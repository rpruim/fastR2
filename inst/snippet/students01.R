act.glm <- glm(grad ~ ACT, data = Students, family = "binomial")
fit1 <- makeFun(act.glm)
gf_point(prop.grad ~ ACT.decile, 
         data = Students %>% 
           mutate(ACT.decile = ntiles(ACT, 10, format = "mean")) %>%
           group_by(ACT.decile) %>% 
           summarise(prop.grad = prop(grad))
) %>% 
  gf_fun(fit1(act) ~ act)

