MoreTosses <- data.frame(heads = rbinom(1000, 1000, 0.5))
histogram( ~ heads / 1000, data = MoreTosses,
           main = "Results of 1000 simulations of 1000 coin tosses",
           xlim = c(0.44, 0.56),
           xlab = "proportion heads")
LotsMoreTosses <- data.frame(heads = rbinom(1000, 10000, 0.5))
histogram( ~ heads / 10000, data = LotsMoreTosses,
           main = "Results of 1000 simulations of 10,000 coin tosses",
           xlim = c(0.44,0.56),
           xlab = "proportion heads")

