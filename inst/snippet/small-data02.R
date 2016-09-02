model <- lm(y ~ x , data = SmallData) 
xyplot(y ~ x, data = SmallData, type = c("p", "r"))

