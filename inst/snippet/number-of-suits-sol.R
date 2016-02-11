p<-rep(NA,4)
p[1] <- choose(4,1) * choose(13,5) / choose(52,5)
p[2] <- choose(4,2) * ( choose(26,5) - (choose(13,5)+choose(13,5))) / choose(52,5) 
p[4] <- choose(4,1) * choose(13,2) * 13 * 13 * 13 / choose(52,5)
p[3] <- 1 - sum(p[-3])   # sum of all probabilities must be 1
rbind(1:4,p)

