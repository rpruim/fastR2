greeting <- "hello, world!"
save(SomeData, greeting, file="saved_objects.zip")  # saves both objects in 1 file
load("saved_objects.zip")                           # loads them both

