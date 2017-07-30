h <- hatvalues(small.model); h
range(h)   # should be contained in [0,1]
sum(h)     # should be p = 2
n <- nrow(SmallData)
with(SmallData, 1/n + (x - mean(x))^2/sum((x - mean(x))^2))

