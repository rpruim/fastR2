ice.trt <- lm(t1930 - b1930 ~ treatment * location, data = Ice)
anova(ice.trt)

