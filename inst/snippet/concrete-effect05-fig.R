lm(strength ~ limestone + water + limestone:water, 
   data = Concrete) %>%
  Effect(c("water", "limestone"), . , partial.residuals = TRUE) %>%
  plot("water")

