coagulation %>%
  group_by(diet) %>%
  summarise(n=n(), mean = mean(coag), SS = sum((coag - mean(coag))^2))
coagulation <- 
  coagulation %>% 
  group_by(diet) %>%
  mutate(group_mean = mean(coag))

grandMean <- mean( ~ coag, data = coagulation); grandMean
groupMean <- coagulation$group_mean; groupMean
SST <- sum((coagulation$coag - grandMean)^2); SST  # total variation
SSE <- sum((coagulation$coag - groupMean)^2); SSE  # w/in group variation
SSM <- sum((groupMean - grandMean)^2 ); SSM        # b/w group variation

