# score interval using uniroot:
p.hat <- 115/200; n <- 200;
f <- function(p) {
    abs(p.hat - p) / sqrt(p * (1-p) / n) + qnorm(0.025);
}
uniroot(f, c(0, p.hat))$root;
uniroot(f, c(p.hat, 1))$root;
uniroot(f, c(0, p.hat))$estim.prec

