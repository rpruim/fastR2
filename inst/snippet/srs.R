sample(1:30, 15)                   # 15 random numbers in 1-30
set.seed(123)
sample(1:30, 15, replace = TRUE)   # iid random sample
set.seed(123)
resample(1:30, 15)                 # same iid random sample

