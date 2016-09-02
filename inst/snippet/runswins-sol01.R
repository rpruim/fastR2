BB <- 
  MLB2004 %>% 
  mutate(
    runmargin = (R - OR) / G,
    winP = W / G)
glm.bb <-
  glm(cbind(W, L) ~ runmargin, data = BB, family = binomial()) 
lm.bb <- lm(winP ~ runmargin, data = BB) 
msummary(lm.bb)
BB <- BB %>% 
  mutate(
    glmPredWinP = makeFun(glm.bb)(runmargin = runmargin), 
    lmPredWinP = makeFun(lm.bb)(runmargin = runmargin)
    )
plot(lm.bb, w = 2)
plot(glm.bb, w = 2)
# observations 8 and 27 have largest residuals
BB[c(8, 27, 1:2, 29:30), c("team", "winP", "glmPredWinP", "lmPredWinP")]

