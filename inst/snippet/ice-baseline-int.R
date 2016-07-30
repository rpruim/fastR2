base.lmint <- lm(b1930 ~ location * treatment, data = Ice)
anova(base.lmint)

