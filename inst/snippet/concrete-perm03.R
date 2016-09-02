Null.F <- 
  do(5000) * lm(shuffle(strength) ~ limestone + water, data = Concrete)
Null.F %>% head(3)
prop( ~ (F > summary(concrete.lm)$fstat[1]), data = Null.F) 

