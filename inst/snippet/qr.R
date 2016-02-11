glimpse(SmallData)
model <- lm(y ~ x, data = SmallData)
Q <- qr.Q(qr(model))    # this is Q1
R <- qr.R(qr(model))    # this is R1

