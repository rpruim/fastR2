base.lmadd <- 
  lm(temp ~ location + treatment, data = Ice1930 %>% filter(phase == "b"))
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

