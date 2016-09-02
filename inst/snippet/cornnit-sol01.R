data(cornnit, package = "faraway")
xyplot(yield ~ log(1 + nitrogen), data = cornnit, type = c("p", "r"))
cornnit.mod <- lm(yield ~ log(1 + nitrogen), data = cornnit)
msummary(cornnit.mod)
plot(cornnit.mod, w = 1:3)

