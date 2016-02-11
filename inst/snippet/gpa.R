# fit some models
gpa.lm <- lm(gpa~satm+satv+act,gpa)
gpa.lma <- lm(gpa~ -1 + satm+satv+act,gpa)
gpa.lmb <- lm(gpa~satv+act,gpa)
gpa.lmc <- lm(gpa~satm+act,gpa)
gpa.lmd <- lm(gpa~satm+satv,gpa)
gpa.lme <- lm(gpa~1,gpa)

