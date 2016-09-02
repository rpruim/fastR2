xyplot(yield^2 ~ sqrt(nitrogen), data = cornnit, type = c("p", "r"))
cornnit.mod3 <- lm(yield^2 ~ sqrt(nitrogen), data = cornnit)
msummary(cornnit.mod3)
plot(cornnit.mod3, w = 1:3)

