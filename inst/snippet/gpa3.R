gpa.lm2<- lm(satm~satv+act,gpa); summary(gpa.lm2)
gpa.lm3<- lm(satm~satv,gpa); summary(gpa.lm3)
gpa.lm4<- lm(satm~act,gpa); summary(gpa.lm4)

