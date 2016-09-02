f1.glm2 <- 
    glm(factor(t2d) ~ Gdose + sex, data = Fusion1m, 
        family = binomial())
msummary(f1.glm2)

