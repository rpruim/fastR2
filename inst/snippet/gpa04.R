gpa.lm5 <- lm(gpa ~ act + satv, data = GPA); msummary(gpa.lm5)
gpa.lm6 <- lm(satv ~ act, data = GPA); msummary(gpa.lm6)

