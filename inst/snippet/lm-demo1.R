x <- 1:4; y <- c(2,3,5,6)
model <- lm(y~x) 
xyplot(y~x, type = c("p", "r"))

