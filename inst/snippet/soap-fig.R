fit1 <- makeFun(Soap.model1)
# transformation required to undo transformation on y
fit2 <- makeFun(Soap.model2, transformation = function(x) x^3)
gf_point(weight ~ day, data = Soap) %>%
  gf_function(fun = fit1, color = "navy") %>%
  gf_function(fun = fit2, color = "red") 

