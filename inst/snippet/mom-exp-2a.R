time <- c(49.0,60.4,8.9,43.4,34.8,8.2,13.6,11.5,99.4,31.9)  
mean(time)
lambda.hat = 1/mean(time); lambda.hat

histogram(~time,
    n=10,
    density=T, dmath=dexp,
    args=list(rate=lambda.hat)
    )
breaks = seq(0, 12, by=2)^2
histogram(~time,
    n=10,
    breaks=breaks,
    density=T, dmath=dexp,
    args=list(rate=lambda.hat)
    )

