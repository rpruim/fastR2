require(faraway)
data(seatpos, package="faraway")
seatpos.lm1=lm(hipcenter ~ ., data=seatpos)
summary(seatpos.lm1)

