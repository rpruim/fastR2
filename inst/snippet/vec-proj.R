x <- c(1, 2, 3); v <- c(1, 1, 1)
project(x, v) 
dot(x, v) * v / vlength(v)^2
project(x, v, type = 'coef') 
dot(x, v) / vlength(v)^2
project(x, v, type = 'length') 
dot(x, v) / vlength(v)

