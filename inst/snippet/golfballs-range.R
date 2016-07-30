stat <- function(x) { diff(range(x)) }  
statTally(golfballs, rgolfballs, stat,
	      xlab = "test statistic (range)")

