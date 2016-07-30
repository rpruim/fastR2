# fit some models
gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
gpa.lma <- lm(gpa ~ -1 + satm + satv + act, data = GPA)
gpa.lmb <- lm(gpa ~ satv + act, data = GPA)
gpa.lmc <- lm(gpa ~ satm + act, data = GPA)
gpa.lmd <- lm(gpa ~ satm + satv, data = GPA)
gpa.lme <- lm(gpa ~ 1, data = GPA)

