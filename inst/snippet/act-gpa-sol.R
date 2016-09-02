grades <- ACTgpa
t.test(grades$ACT)
t.test(grades$GPA)
grades.model <- lm(GPA ~ ACT, data = grades)
msummary(grades.model)
grades.plot1 <- xyplot(GPA ~ ACT, data = grades, panel = panel.lmbands)
act2gpa <- makeFun(grades.model)
act2gpa(ACT = 25, interval = "confidence")
act2gpa(ACT = 25, interval = "prediction")

