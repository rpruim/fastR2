# find sample size with 90% power
uniroot(function(size){ as.numeric(power(size)[1]) -0.90}, c(400, 5000))

