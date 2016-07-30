f1.glm2 <- 
    glm(factor(t2d) ~ Gdose + sex, data = Fusion1m, family = binomial())
f1.glm2
summary(f1.glm2)

