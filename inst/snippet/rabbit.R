require(faraway); data(rabbit,package="faraway")
rabbit.lm1 <- lm(gain~block+treat,rabbit)
rabbit.lm2 <- lm(gain~treat+block,rabbit)
rabbit.lm3 <- lm(gain~block,rabbit)
rabbit.lm4 <- lm(gain~treat,rabbit)

