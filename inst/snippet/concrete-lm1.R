require(Devore7); data(xmp13.13, package='Devore7')
concrete <- data.frame(
    limestone=xmp13.13$x1,
    water=xmp13.13$x2,
    strength=xmp13.13$strength)
concrete.lm1 <- lm(strength ~ limestone + water, concrete)
concrete.lm1

