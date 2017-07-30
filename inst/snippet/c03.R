x <- c(first = 10, second = 20); x
names(x)                        # what are the names?
x["first"]
x[1]
y <- 1:3                        # vector without names
names(y) <- c("A", "B", "C")    # names added
y
as.vector(y)                    # vector without the names

