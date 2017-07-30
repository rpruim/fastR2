R <- t(Q) %*% X; R %>% round(4)   # should be upper triangular
Q %*% R %>% round(4)              # should be X
X

