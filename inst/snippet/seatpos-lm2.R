seatpos.lm2 <- lm(hipcenter ~ Age + Weight + Ht, data = seatpos)
summary(seatpos.lm2)
faraway::vif(seatpos.lm2)

