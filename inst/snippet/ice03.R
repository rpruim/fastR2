Ice1930 <- Ice2 %>% filter(time == 1930)
base.lmint <- 
  lm(temp ~ location * treatment, data = Ice1930 %>% filter(phase == "b"))
anova(base.lmint)

