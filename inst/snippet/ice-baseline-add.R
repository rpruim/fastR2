base.lmadd <- lm(b1930 ~ location + treatment, data = Ice)
anova(base.lmadd)
xplot(base.lmadd, w = c(5, 2))

