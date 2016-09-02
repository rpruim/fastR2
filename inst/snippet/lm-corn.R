# the Corn data frame has an inconvenient "shape" 
# (each type of Corn is in its own column)
head(Corn, 3)                                   
# this puts all the yields in one column and type of seed in another
Corn2 <- stack(Corn)                         
Corn2[c(1, 2, 12, 13), ]
# the default variable names aren't great, so we rename them
names(Corn2) <- c("yield", "treatment")       
Corn2[c(1, 2, 12, 13), ]
favstats(yield ~ treatment, data = Corn2)
Corn.model <- lm(yield ~ treatment, data = Corn2)
msummary(Corn.model)

