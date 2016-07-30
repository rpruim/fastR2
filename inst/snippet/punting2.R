summary(lm(rFlexibility ~ rStrength, data = Punting))
xyplot(rStrength ~ rFlexibility, data = Punting)
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r
r^2

