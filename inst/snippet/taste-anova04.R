taste.lm <- lm(score ~ scr * liq, data = TasteTest)
msummary(taste.lm)

