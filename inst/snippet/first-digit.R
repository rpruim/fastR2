firstDigit <- function(x) {
    trunc(x / 10^(floor(log10(abs(x)))))
}
# lengths (mi) of 141 major North American rivers
y0 <- log10(2:10) - log10(1:9)
y1 <- tally( ~ firstDigit(rivers), format = "prop")         # lengths in miles
y2 <- tally( ~ firstDigit(rivers * 1.61), format = "prop")  # lengths in km
xyplot(y0 + y1 + y2 ~ 1:9, auto.key = list(space = "right"), type = c("p", "l"))

