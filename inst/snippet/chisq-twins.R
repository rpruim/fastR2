convictions <- rbind(dizygotic   = c(2, 15), 
                     monozygotic = c(10, 3))
colnames(convictions) <- c('convicted', 'not convicted')
convictions
chisq.test(convictions, correct = FALSE)
pval(chisq.test(convictions))
pval(fisher.test(convictions))

