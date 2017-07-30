f <- function(x,y) x^2 * y / 84
vals <- 2:6
probs <- c(
  "2" = f(1,1),
  "3" = f(1,2) + f(2,1),
  "4" = f(1,3) + f(2,2) + f(3,1),
  "5" = f(2,3) + f(3,2),
  "6" = f(3,3)
)
probs
probs %>% fractions()
# check that total probability = 1
sum(probs)
# E(S)
sum(vals * probs)

