# simulated data sets
testStats <- replicate(numSims, {
    tally(twin ~ shuffle(conviction), data = FT)[1, 1]
    })

# for p-value 
tally(testStats)
# tail probabilities
prop1(testStats >= 15)
# 2-sided p-value
2 * prop1(testStats >= 15)

