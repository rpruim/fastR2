greeting <- "hello, world!"
save(SomeData, greeting, file = "mystuff.rda")  # saves both objects in 1 file
load("mystuff.rda")                             # loads them both

