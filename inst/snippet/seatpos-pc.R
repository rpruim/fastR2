pc=with(seatpos,princomp(cbind(HtShoes,Ht,Seated,Arm,Thigh,Leg),
    scores=T))
summary(pc, loadings=T)
seatpos.lmpc <-lm(hipcenter ~ Age + Weight + pc$scores[,1], data = seatpos)
summary(seatpos.lmpc)
faraway::vif(seatpos.lmpc)

