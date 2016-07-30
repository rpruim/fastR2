# P(55 <= X <= 65)
pbinom(65, 100, 0.6) - pbinom(54, 100, 0.6)        
# without continuity correction:
diff(pnorm(c(55, 65), 60, sqrt(100 * 0.6 * 0.4)))
# with continuity correction:
diff(pnorm(c(54.5, 65.5), 60, sqrt(100 * 0.6 * 0.4)))

