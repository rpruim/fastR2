Null.F <- 
  do(5000) * lm(shuffle(strength) ~ limestone + water, data = Concrete)
Null.F %>% head(3)
prop1( ~ (F >= obsF), data = Null.F) 

