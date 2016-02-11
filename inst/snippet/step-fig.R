step.model <- lm(HR - restHR ~ height * freq, step)
summary(step.model)
anova(step.model)
xyplot(HR - restHR ~ freq, data=step, groups = height, type='a')

