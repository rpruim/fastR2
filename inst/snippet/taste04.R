M <- cbind(                                # model matrix
        "C1" = rep(c(-1, -1, 1, 1), each = 4)/8,     # C1
        "C2" = rep(c(-1, 1, -1, 1), each = 4)/8,     # C2
        "C3" = rep(c(1, -1, -1, 1), each = 4)/4      # C3
        )
taste.lm2 <- lm(score ~ M, data = TasteTest)
msummary(taste.lm2)

