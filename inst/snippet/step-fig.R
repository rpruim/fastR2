step.model <- lm(HR - restHR ~ height * freq, data = Step)
summary(step.model)
anova(step.model)
xyplot(HR - restHR ~ freq, data = Step, groups = height, type='a')

