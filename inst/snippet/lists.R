L <- list(a = 5, b = "X", c = 2.3, d = c("A", "B")); mode(L); length(L)
L[[4]]        # 4th element of list
L[["d"]]      # element named "d"
L$d           # element named "d"
L[["d"]] <- 1:3; str(L)
L[["b"]] <- NULL; str(L)     # removing an item from a list

