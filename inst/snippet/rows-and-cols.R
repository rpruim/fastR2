DDD <- data.frame(number = 1:5, letter = letters[1:5])
dim(DDD)
nrow(DDD)
ncol(DDD)
names(DDD)
row.names(DDD)
row.names(DDD) <- c("Abe", "Betty", "Claire", "Don", "Ethel")
DDD                 # row.names affects how a data.frame prints

