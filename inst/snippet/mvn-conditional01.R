B <- rbind(c(1, 0), c(1, 1)); B
C <- rbind(c(1,2)); C
Binv <- solve(B); Binv
C %*% Binv

