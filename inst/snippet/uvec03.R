pList <- lapply(ulist, function(u) project(x, u)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x,x))[2:4])
3 * var(x)

