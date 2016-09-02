histogram( ~ Estimate | factor(.row), data = Null.t, scales = "free",
           v = c(0, mean(~strength, data = Concrete)))

