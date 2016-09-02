step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
xyplot(HR - restHR ~ freq, data = Step, groups = height, type = "a")

