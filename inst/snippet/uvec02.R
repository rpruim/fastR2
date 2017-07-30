ulist <- list(u1, u2, u3, u4)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
c(dot(u1, u2), dot(u1, u3), dot(u1, u4), 
  dot(u2, u3), dot(u2, u4), dot(u3, u4))

