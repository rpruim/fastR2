update <- function(theta, alpha, power) {
  theta * power  / (theta * power + alpha * (1-theta))
}
# gf_function(fun = update, args = list(alpha = 0.05, power = 0.66), xlim = c(0,1)) %>%
#   gf_labs(
#     y = expression(paste("updated probability that ", H[0], " is false")),
#     x = expression(paste("prior probability that ", H[0], " is false (", theta, ")"))
#   )
plotFun(update(theta, alpha = 0.05, power = 0.66) ~ theta,
        theta.lim = c(0,1),
        ylab = expression(paste("updated probability that ", H[0], " is false")),
        xlab = expression(paste("prior probability that ", H[0], " is false (", theta, ")")))
plotFun(update(theta, alpha = 0.05, power = 0.90) ~ theta,
        add = TRUE, col = "red")
plotFun(update(theta = 0.1, alpha, power = 0.66) ~ alpha,
        alpha.lim = c(0, 0.10),
        ylab = expression(paste("updated probability that ", H[0], " is false")),
        xlab = expression(paste("significance level (", alpha, ")")))
plotFun(update(theta = 0.1, alpha, power = 0.90) ~ alpha,
        add = TRUE, col = "red")

