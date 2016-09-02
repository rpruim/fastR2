data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
xyplot(coag ~ diet, coagulation)
bwplot(coag ~ diet, coagulation)

