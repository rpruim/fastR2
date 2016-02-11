x <- c(3, 4, 4, 7, 7)
mean(x)
v <- x - mean(x)
u1 <- c(1,1,1,1,1) / sqrt(5)
u2 <- c(1,-1,0,0,0) / sqrt(2)
u3 <- c(1,1,-2,0,0) / sqrt(6)
u4 <- c(1,1,1,-3,0) / sqrt(12)
u5 <- c(1,1,1,1,-4) / sqrt(20)
ulist <- list(u1,u2,u3,u4,u5)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
xList <- lapply( ulist, function(u)  project(x,u) ); xList
vList <- lapply( ulist, function(u)  project(v,u) ); vList
all.equal(xList[-1], vList[-1])

