u <- c(first = 1, second = 2, third = 3)
u                                # names are displayed in output
names(u)                         # show just the names
u["second"]                      # access by name
names(u) <- c("one", "two", "three") # change the names
names(u)[2] <- "TWO"             # change just one name
u
u["first"]                       # old names gone now
setNames(u, c("a", "b", "c"))    # new object is named
u                                # u remains unchanged

