w <- 2.5; mode(w); length(w)
x <- c(1, 2); mode(x); length(x)
y <- "foo"; mode(y); length(y)
y[1]; y[2]             # not an error to ask for y[2]
z <- TRUE; mode(z); length(z)
abc <- letters[1:3]
abc; mode(abc); length(abc)
abc[3]
abc[6] <- "Z"; abc     # NAs fill in to make vector long enough

