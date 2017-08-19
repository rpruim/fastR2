data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
gf_point(coag ~ diet, data = coagulation)
gf_boxplot(coag ~ diet, data = coagulation)

