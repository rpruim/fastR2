data(eggprod, package = "faraway")
eggprod.lm <- lm(eggs ~ block + treat, data = eggprod)
anova(eggprod.lm)

