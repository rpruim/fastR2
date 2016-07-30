taste.lm <- lm(score ~ scr * liq, data = TasteTest)
anova(taste.lm)

