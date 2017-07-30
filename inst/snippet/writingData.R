args(write.table) 
SomeData <- data.frame(x = 1:3, y = LETTERS[1:3])
SomeData
write.table(SomeData, "SomeData.txt")
write.csv(SomeData, "SomeData.csv")
# this system call should work on a Mac or Linux machine
system("head SomeData.txt SomeData.csv")

