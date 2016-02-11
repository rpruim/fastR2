seatpos.lm2=lm(hipcenter ~ Age + Weight + Ht, seatpos )
summary(seatpos.lm2)
vif(seatpos.lm2)

