birthdays69 <- rep(Births78$date, Births78$births)
sharedBirthday <- function(n, birthdays) {
	bdays <- sample( birthdays, n )
	length( unique(bdays) ) < n  
}
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays69)) 

# 1969
Births69 <- Births %>% filter(year == 1969)
birthdays69 <- rep(Births69$date, Births69$births)
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays69)) 

# 1988
Births88 <- Births %>% filter(year == 1988)
birthdays88 <- rep(Births88$date, Births88$births)
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays88)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays88)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays88)) 

