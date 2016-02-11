ml <- maxLik(loglik1, start = 0.5, x = counts); ml
theta.hat <- coef(ml)

loglik <- function(theta, x) {
    if (theta < 0 || theta > 1) { return (NA) }
    dmultinom(x, size = sum(x), prob = theta2probs(theta), log = TRUE)
}

ml <- maxLik(loglik, start = 0.5, x = counts); ml
theta.hat <- coef(ml); theta.hat

