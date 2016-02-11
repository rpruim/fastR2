tally(~Student+Parents, data=familySmoking, margin=FALSE) -> smokeTab; 
smokeTab
chisq.test(smokeTab)
observedStat <- chisq.test(smokeTab)$stat
stats <- do(2000) * 
  stat( chisq.test( tally(~shuffle(Student)+Parents, data=familySmoking, margin=FALSE)) )
stats <- stats$`X-sqhared`
sum( stats > observedStat ) -> x; x / length(stats)   # p-value
binom.test(x,length(stats),alternative="less")$conf.int

