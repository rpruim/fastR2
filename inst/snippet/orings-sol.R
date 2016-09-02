orings.logit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = logit))
orings.probit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = probit))
confint(orings.logit)
confint(orings.probit)
g.logit <- makeFun(orings.logit) 
g.probit <- makeFun(orings.probit) 
plotFun(g.logit(t) ~ t, t.lim = c(25, 90))
plotFun(g.probit(t) ~ t, add = TRUE, 
        lty = 2, lwd = 5, alpha = .4, col = "black")

