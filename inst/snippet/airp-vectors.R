u1 <- 1/2 * c(1, 1, -1, -1, 0, 0)
u2 <- 1 / sqrt(12) * c(1, 1, 1, 1, -2, -2)
dot(AirPollution$pollution, u1)
dot(AirPollution$pollution, u2)
t1 <- dot(AirPollution$pollution, u1) / sqrt(202/3); t1
t2 <- dot(AirPollution$pollution, u2) / sqrt(202/3); t2
t1^2
t2^2
2 * pt( - abs(t1), df = 3)
2 * pt( - abs(t2), df = 3)

