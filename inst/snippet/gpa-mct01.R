# fit some models
#
gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
gpa.lma <- lm(gpa ~ -1 + satm + satv + act, data = GPA)
#
# model comparison tests for 5 p-values in msummary(gpa.lm)
#
anova(gpa.lma, gpa.lm)

