birthdays <- rep(Births78$dayofyear, Births78$births)
sharedBirthday <- function( n, birthdays) {
	bdays <- sample( birthdays, n )
	length( unique(bdays) ) < n  
}
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays)) 

