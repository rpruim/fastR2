Soap2 <- Soap %>% filter(day < 20)
Soap.model2 <- lm(weight ~ day, data = Soap2)
msummary(Soap.model2)
plot(Soap.model2, w = 1:2)

