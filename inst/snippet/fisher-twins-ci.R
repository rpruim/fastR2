binom.test(2 * sum(testStats >= 15), numSims) %>% confint()  
binom.test(sum(testStats >= 15 | testStats <= 5), numSims) %>% confint()  

