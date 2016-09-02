xyplot(yield ~ log(1 + nitrogen), data = cornnit[-21, ], type = c("p", "r"))
cornnit.mod2 <- lm(yield ~ log(1 + nitrogen), data = cornnit[-21, ])
msummary(cornnit.mod2)
plot(cornnit.mod2, w = 1:3)

