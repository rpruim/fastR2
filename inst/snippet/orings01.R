# select the version of this data set in the faraway package
data(orings, package = "faraway")        
orings <-
  orings %>% mutate(failure = damage != 0)   # convert to binary response
orings.model <- 
    glm(failure ~ temp, data = orings, family = binomial(link = logit))
msummary(orings.model)

