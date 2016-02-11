# trace=0 turns off intermediate reporting
seatpos.lmstep<-step(seatpos.lm1, trace=0)  
summary(seatpos.lmstep)
vif(seatpos.lmstep)
anova(seatpos.lm1,seatpos.lmstep)

