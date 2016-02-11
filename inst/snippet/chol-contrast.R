confint(glht(chol.model, mcp(trt = 
    rbind(
        "new - old" = c(2,2,2,-3,-3)/6)
    )))

