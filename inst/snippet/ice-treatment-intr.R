ice.trt2 <- lm(t1930 - b1930 ~ treatment, data = Ice,
                 subset = location == 'intramuscular')
summary(ice.trt2)

confint(glht(ice.trt2, mcp(treatment = 'Tukey')), level = 0.90)

