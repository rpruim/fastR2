iris %>% group_by(Species) %>%
  do(data.frame(as.list( 
	 confint(t.test( ~ Sepal.Length, data= .))
  )))

