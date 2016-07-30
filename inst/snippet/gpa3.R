gpa.lm2 <- lm(satm ~ satv + act, data = GPA); summary(gpa.lm2)
gpa.lm3 <- lm(satm ~ satv, data = GPA); summary(gpa.lm3)
gpa.lm4 <- lm(satm ~ act, data = GPA); summary(gpa.lm4)

