histogram( ~ pulse, data = LittleSurvey)
histogram( ~ pulse, data = LittleSurvey, subset = pulse > 30) 
pulseSubset <- filter(LittleSurvey, pulse > 30)
mean(~ pulse, data = pulseSubset)
median(~ pulse, data = pulseSubset)

