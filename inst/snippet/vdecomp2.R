vdecomp2 <- function(x, v1, v2) {
  w1 <- v1 - project(v1, v2); w2 <- v2 - project(v2, v1) 
  p1 <- project(x, w1);       p2 <- project(x, w2) 
  # need to be careful about 0 vectors
  a  <- if (vlength(w1) == 0) { 0 } else {
    sign(dot(w1, p1)) * vlength(p1) / vlength(w1)
  }
  b  <- if (vlength(w2) == 0) { 0 } else { 
    sign(dot(w2, p2)) * vlength(p2) / vlength(w2)
  }
  list( 
    coefficients = c(a, b), 
    projection   = a * v1 + b * v2,
    remainder    = x - a * v1 - b * v2
  )
}

