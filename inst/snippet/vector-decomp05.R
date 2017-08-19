v1 <- c(1, 0, 0)
v2 <- c(1, 1, 1)
v3 <- c(1, 2, 3)
x  <- c(2, 7, 3)
vdecomp(x, v1, v2, v3) %>% lapply(round, digits = 8)

