# note the different order of the arguments and 
# different output formats in these two functions.
quantile( ~eruptions, data = faithful, probs = (0:10)/10)
qdata( ~ eruptions, (0:10)/10, data = faithful)
quantile( ~ eruptions, probs = (0:4)/4, data = faithful)
qdata( ~ eruptions, (0:4)/4, data = faithful)

