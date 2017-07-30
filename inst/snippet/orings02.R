# by default, predict() works on the linear model scale
r <- predict(orings.model, newdata = data.frame(temp = 31)); r
ilogit(r)
# but we can ask for it to work on the "response" scale
predict(orings.model, newdata = data.frame(temp = 31), type = "response")

