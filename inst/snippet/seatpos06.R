# trace=0 turns off intermediate reporting
seatpos.lmstep <- step(seatpos.lm1, trace = 0)  
msummary(seatpos.lmstep)
faraway::vif(seatpos.lmstep)

