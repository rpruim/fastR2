base.lmadd <- lm(b1930 ~ location + treatment, data = Ice)
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

