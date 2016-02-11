set.seed(1)                      # use fixed "random" seed
someData <- data.frame(x=runif(300),group=factor(rep(1:3,each=100)))
qqmath(~x|group, data=someData, distribution=qunif)
qlogunif <- function(p,a=0,b=1,base=10) {
    -log(1-qunif(p,a,b),base)
}
qqmath(~ -log10(x) | group, data=someData, distribution=qlogunif)

