gf_point(rStrength ~ rFlexibility, data = Punting)
lm(rFlexibility ~ rStrength, data = Punting) %>% msummary()
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r 
r^2

