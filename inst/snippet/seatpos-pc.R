pc = with(seatpos, 
          princomp(cbind(HtShoes, Ht, Seated, Arm, Thigh, Leg),
                   scores = TRUE))
msummary(pc, loadings = TRUE)
seatpos.lmpc <-lm(hipcenter ~ Age + Weight + pc$scores[, 1], data = seatpos)
msummary(seatpos.lmpc)
faraway::vif(seatpos.lmpc)

