odds <- exp(coef(buck.model)[1]); odds     # odds when conc = 0
# prob when conc = 0
odds / (1 + odds)
ilogit(coef(buck.model)[1])  
dead(0)

