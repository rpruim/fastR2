binom_power <- function(n, p_alt, alpha = 0.05) {
  p_null <- 0.50
  # reject if X <= critical_lo or X >= critical_hi
  critical_lo <- qbinom(    alpha/2, size = n, prob = p_null) - 1
  critical_hi <- qbinom(1 - alpha/2, size = n, prob = p_null) 
  pbinom(critical_lo, n, p_alt) + 1 - pbinom(critical_hi - 1, n, p_alt)  
}

PowerData <- 
  expand.grid(n = seq(5, 10000, by = 5), p_alt = c(0.52, 0.55, 0.60)) %>%
  mutate( 
    power = binom_power(n, p_alt),
    plab = paste("alt prob =", as.character(p_alt))
  )
  
gf_line(power ~ n | plab, data = PowerData, size = 1) %>%
  gf_labs(y = "power", x = "number of coin tosses") %>%
  gf_lims(y = c(0, 1.1))

