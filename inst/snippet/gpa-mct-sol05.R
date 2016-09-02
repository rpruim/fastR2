# combined SAT verses subscore
gpa.lmf <- lm(gpa ~ I(satv + satm) + act, data = GPA)
anova(gpa.lmf, gpa.lm)

