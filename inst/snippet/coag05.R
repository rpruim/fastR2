data(coagulation, package = "faraway")
# group-by-group tally
coagulation %>%
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>%
  summarise(n = n(), group.mean = mean(coag), 
            SSE = sum((coag - group.mean)^2),
            SSM = sum((group.mean - grand.mean)^2),
            SST = sum((coag - grand.mean)^2))

# individual tally
coagulation <- 
  coagulation %>% 
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>% mutate(group.mean = mean(coag)) %>%
  ungroup()
coagulation %>% sample(5)

data.frame(
  SST = sum( ~ (coag - grand.mean)^2, data = coagulation),
  SSE = sum( ~ (coag - group.mean)^2, data = coagulation),
  SSM = sum( ~ (group.mean - grand.mean)^2, data = coagulation))

