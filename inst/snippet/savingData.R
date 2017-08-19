greeting <- "hello, world!"
save(SomeData, greeting, file = "mystuff.rda")  # saves both in 1 file
load("mystuff.rda")                             # loads both

