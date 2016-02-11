require(vcd)
mosaic( ~ victim + defendant + death, data = DeathPenalty)
structable(~ victim + defendant + death, data = DeathPenalty)

