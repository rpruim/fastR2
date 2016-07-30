ab <- 
  BTabilities(NCAA.model2) 
ratings <- 
  ab[order(-ab[, "ability"]), ]
ratings[1:13, ]

