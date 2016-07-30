# define a function to calculate power for given sample size.
power <- function(size, null = 0.50, alt = 0.55){ 
    leftCritical  <- -1 + qbinom(0.025, round(size), null)
    rightCritical <-  1 + qbinom(0.975, round(size), null)
    alpha <- 1 - pbinom(rightCritical - 1, round(size), null) + 
                 pbinom( leftCritical,     round(size), null)
    leftPower  <-     pbinom(leftCritical,      round(size), alt)
    rightPower <- 1 - pbinom(rightCritical - 1, round(size), alt)

    data.frame(power = leftPower + rightPower,
      leftPower = leftPower,
      rightPower = rightPower,
      null = null,
      alt = alt,
      leftCritcal = leftCritical,
      rightCritcal = rightCritical,
      alpha = alpha
    )
}

