ice.trt2 <- lm(T1930 - B1930 ~ Treatment, ice,
                 subset=Location=='intramuscular')
summary(ice.trt2)

confint(glht(ice.trt2, mcp(Treatment='Tukey')),level=0.90)

