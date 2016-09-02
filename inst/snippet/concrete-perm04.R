Null.t2 <- 
  do(10000) * 
    lm(strength ~ shuffle(limestone) + water, data = Concrete) %>%
    coef() %>% getElement(2)
  
Null.t3 <- 
  do(10000) * {
      lm(strength ~ limestone + shuffle(water), data = Concrete) %>%
      coef() %>% getElement(3)
  }
Null.t2 %>% head(3)
2 * prop( ~ (result >= coef(concrete.lm)[2]), data = Null.t2)
2 * prop( ~ (result <= coef(concrete.lm)[3]), data = Null.t3)

