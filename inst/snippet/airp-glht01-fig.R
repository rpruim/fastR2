require(multcomp)   
airp.cint <- confint(glht(airp.lm, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.lm), system = "gg") %>% 
  gf_theme(legend.position = "top") %>%
  gf_labs(title = "")
  
# plot(airp.cint)
# mplot(TukeyHSD(airp.lm), system = "gg") 

