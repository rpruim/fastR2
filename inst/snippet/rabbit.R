data(rabbit, package = "faraway")
rabbit.lm1 <- lm(gain ~ block + treat, data = rabbit)
rabbit.lm2 <- lm(gain ~ treat + block, data = rabbit)
rabbit.lm3 <- lm(gain ~ block, data = rabbit)
rabbit.lm4 <- lm(gain ~ treat, data = rabbit)

