# the Corn data frame has an inconvenient "shape" 
# (each type of Corn is in its own column)
head(Corn, 3)                                   
require(tidyr)
# this puts all the yields in one column and type of seed in another
Corn2 <- Corn %>% gather(key = "treatment", value = "yield")
# inspect a few rows in each treatment group
Corn2 %>% group_by(treatment) %>% do(head(., 3))

