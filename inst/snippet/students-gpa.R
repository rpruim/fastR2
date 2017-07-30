act.glm <- 
  glm(grad ~ ACT, data = Students, family = "binomial")
gpa.glm <- 
  glm(grad ~ hsGPA, data = Students, family = "binomial")
actgpa.glm <- 
  glm(grad ~ ACT + hsGPA, data = Students, family = "binomial")
msummary(actgpa.glm) %>% coef()
c(gpa    = deviance(gpa.glm),
  act    = deviance(act.glm),
  actgpa = deviance(actgpa.glm))
# small p-value suggests that adding gpa is helpful
1 - pchisq(deviance(act.glm) - deviance(actgpa.glm), df = 2)
# larger p-value here compared with act.glm suggests better fit
1 - pchisq(deviance(actgpa.glm), df = df.residual(actgpa.glm))

