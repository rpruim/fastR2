gpa.lm5 <- lm(gpa ~ act + satv, data = GPA); summary(gpa.lm5)
gpa.lm6 <- lm(satv ~ act, data = GPA); summary(gpa.lm6)

