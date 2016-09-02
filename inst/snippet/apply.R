sapply(KidsFeet, class)    # determine the class of each variable
lapply(iris, function(x) if (is.numeric(x)) favstats(x) else tally(x))
M <- rbind(1:3, 4:6, 7:9); M
apply(M, 1, sum)           # row sums
rowSums(M)                 # dedicated row sums function
# tapply version of mean(length ~ sex, data = KidsFeet)
tapply(KidsFeet$length, KidsFeet$sex, mean)

