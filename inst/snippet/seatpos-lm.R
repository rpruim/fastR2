data(seatpos, package = "faraway")
seatpos.lm1 <- lm(hipcenter ~ ., data = seatpos)
msummary(seatpos.lm1)

