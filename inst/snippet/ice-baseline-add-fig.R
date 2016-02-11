base.lmadd <- lm(B1930 ~ Location + Treatment, ice)
anova(base.lmadd)
xplot(base.lmadd, w=c(5,2))

