# This was originally from 
# concrete <- 
#   with(Devore7::xmp13.13, 
#        data.frame(
#          limestone = x1, 
#          water = x2, 
#          strength = X28)
#   )
# concrete$strength[8:9] <- c(48, 42.3)

Concrete <- read.csv("Concrete.csv")
use_data(Concrete)

library(readxl)
ConcreteAll <- read_excel("~/projects/github/fastR/data-raw/Concrete_Data.xls")
names(ConcreteAll) <- 
  c("cement", "slag", "ash", "water", "superP", "courseAg", "fineAg", "age", "strength")
use_data(ConcreteAll)
                        