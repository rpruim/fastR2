null.glm <- 
  glm(cbind(W, L) ~ 1, data = BB, family = "binomial") 
bb.glm <- 
  glm(cbind(W, L) ~ runmargin, data = BB, family = "binomial") 
saturated.glm <- 
  glm(cbind(W, L) ~ factor(1:30), data = BB, family = "binomial") 

