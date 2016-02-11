require(faraway); data(eggprod,package="faraway")
eggprod.lm <- lm(eggs~block+treat,eggprod)
anova(eggprod.lm)

