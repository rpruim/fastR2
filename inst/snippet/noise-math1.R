model1 <- lm(score~noise+group,mathnoise)
anova(model1)
favstats(score~group, data=mathnoise)

