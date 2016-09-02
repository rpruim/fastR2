gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
msummary(gpa.lm)

