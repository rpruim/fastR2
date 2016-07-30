tally( ~ student + parents, data = FamilySmoking, margin=FALSE) -> smokeTab; 
smokeTab
chisq.test(smokeTab)
observedStat <- chisq.test(smokeTab)$stat
stats <- do(2000) * 
  stat(chisq.test(tally( ~ shuffle(student) + parents, 
                         data = FamilySmoking, margin = FALSE)))
stats <- stats$X.squared
sum(stats >= observedStat) -> x; x / length(stats)   # p-value
binom.test(x, length(stats), alternative = "less")$conf.int

