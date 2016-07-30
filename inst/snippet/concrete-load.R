concrete <- 
  with(Devore7::xmp13.13, 
       data.frame(
        limestone = x1, 
        water = x2, 
        strength = X28)
  )
concrete$strength[8:9] <- c(48, 42.3)

