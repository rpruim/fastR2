# means of bootstrap distributions 
boot.means <- mean( ~ boot.mean | sample, data = Boots)
boot.means
# difference from sample means

boot.means - sample.means 

