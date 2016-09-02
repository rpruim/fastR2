bb.logit <-
  glm(cbind(W, L) ~ runmargin, data = BB, 
      family = binomial(link = logit)) 
bb.probit <-
  glm(cbind(W, L) ~ runmargin, data = BB, 
      family = binomial(link = probit))
confint(bb.logit)
confint(bb.probit)
f.logit <- makeFun(bb.logit) 
f.probit <- makeFun(bb.probit)
plotFun(f.logit(r) ~ r, r.lim = c(-2, 2))
plotFun(f.probit(r) ~ r, add = TRUE, 
        lty = 2, lwd = 5, alpha = .4, col = "black")

