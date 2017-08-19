# means of samples 
sapply(S, mean) 
# means of the bootstrap distributions
mean( ~ boot.mean | sample, data = Boot.Gamma)
# standard deviations of samples 
sapply(S, sd)
# standard error of each sample
sapply(S, sd) / sqrt(16)
# standard deviations of the bootstrap distributions
sd( ~ boot.mean | sample, data = Boot.Gamma)
sqrt(1/8)
# bias
(mean( ~ boot.mean | sample, data = Boot.Gamma) - sapply(S, mean)) / 
   sd( ~ boot.mean | sample, data = Boot.Gamma)

