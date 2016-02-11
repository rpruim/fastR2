data(coagulation,package="faraway")
summary(coag~diet,data=coagulation,fun=favstats)
xyplot(coag~diet,coagulation)
bwplot(coag~diet,coagulation)

