car::scatterplotMatrix(
  ~ Age + Arm + hipcenter + Ht + HtShoes + Leg + Seated + Thigh + Weight, 
  data = seatpos,
  reg.line = lm, smooth = TRUE, span = 0.5, 
  diagonal = 'density')
round(cor(seatpos), 2)

