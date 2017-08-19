confint(glht(chol.lm, mcp(trt = 
    rbind(
        "new - old" = c(2, 2, 2, -3, -3)/6)
    )))

