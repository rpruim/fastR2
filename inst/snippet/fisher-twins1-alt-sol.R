phyper(3,13,17,18)
convictions <- rbind( monozygotic=c(3,10), dizygotic=c(15,2)) 
colnames(convictions) <- c('not convicted','convicted')
convictions
fisher.test(convictions, alternative = "less")

