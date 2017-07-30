prop.test(9458,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9457,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9458,  10000, p = .95) %>% pval()
prop.test(9457,  10000, p = .95) %>% pval()
prop.test(9456,  10000, p = .95) %>% pval()
binom.test(9457, 10000, p = .95) %>% pval()
binom.test(9456, 10000, p = .95) %>% pval()

