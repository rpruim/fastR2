require(Stob)
B <- 10200; B
D <- mean( ~mass, data=Dimes); D
uB <- 100/sqrt(12); uB
uD <- sd(~mass, data=Dimes) / sqrt( nrow(Dimes) ); uD
u <- sqrt( 1/D^2 * uB^2 + B^2/D^4 * uD^2  )

