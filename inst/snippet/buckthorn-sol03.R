odds <- function(p) { p / (1 - p) }
odds(dead(0.01)) / odds(dead(0))
odds(dead(0.30)) / odds(dead(0.29))

