## ----fastR-setup,include=FALSE-------------------------------------------
includeChapter <- rep(TRUE, 7)
includeChapter <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
includeChapter <- rep(c(FALSE, TRUE), c(5, 2))
includeChapter <- rep(TRUE, 7)
includeApp <- rep(TRUE, 5)
includeApp <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

require(MASS)  # make sure this comes before dplyr loads
require(fastR2)
require(mosaic)
theme_set(theme_minimal())
require(knitr)
require(xtable)
options(xtable.floating = FALSE)
opts_knit$set(width=75)
opts_knit$set(self.contained=FALSE)
opts_chunk$set(
  digits = 3,
  dev=c("pdf","postscript"),
  dev.args=list(colormodel="cmyk"),
  comment="##",
  prompt=FALSE,
  size="small",
  cache=TRUE,
  cache.path='cache/c-',
  cache.lazy=FALSE,
  tidy=FALSE,
  fig.width=8*.75,
  fig.height=5*.75,
  fig.show="hold",
  fig.align="center",
  out.width=".47\\textwidth",
#  background="gray88",
#  background="white",
#  background="transparent",
  boxedLabel=TRUE
  )

opts_template$set(fig3 = list(fig.height = 5*.35, fig.width = 8*.35, out.width=".31\\textwidth"))
opts_template$set(fig1 = list(fig.height = 3, fig.width = 8, out.width=".95\\textwidth"))
opts_template$set(figbig = list(fig.height = 9, fig.width = 12, out.width=".95\\textwidth"))

knit_hooks$set(seed = function(before, options, envir) {
    if (before) set.seed(options$seed) 
})

knit_hooks$set(digits = function(before, options, envir) {
    if (before) {
      options(digits = options$digits)
    } else {
      options(digits = 3)
    }
})


knit_hooks$set(
  document = function(x) {
    sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)
    gsub(
      "\\definecolor{shadecolor}{rgb}{0.969, 0.969, 0.969}",
      "\\definecolor{shadecolor}{gray}{0.9}", x, fixed = TRUE)
  }
)

knit_hooks$set(chunk = function (x, options) {
	if ( !is.null(options$boxedLabel) && options$boxedLabel && 
         ! grepl("unnamed-chunk", options$label) &&
		(is.null(options$echo) || options$echo) ) {
		labeling <- paste0( 
			"\\endgraf\\nobreak\\null\\endgraf\\penalty-2\\kern-.5\\baselineskip",
			"\n\n",
			"\\hfill \\makebox[0pt][r]{\\fbox{\\tiny ",
			options$label,
			"}}", 
			"\\endgraf",
			"\\kern-4.5ex\n\n")
	}  else {
		labeling <- ""
	}
    ai = knitr:::output_asis(x, options)
    col = if (!ai)
        paste(knitr:::color_def(options$background), 
              if (!knitr:::is_tikz_dev(options)) "\\color{fgcolor}", 
              sep = "")
    k1 = paste(col, "\\begin{kframe}\n", sep = "")
    k2 = "\\end{kframe}"
    x = knitr:::.rm.empty.envir(paste(k1, labeling, x, k2, sep = ""))
    size = if (options$size == "normalsize")
        ""
    else sprintf("\\%s", options$size)
    if (!ai)
        x = sprintf("\\begin{knitrout}%s\n%s\n\\end{knitrout}",
            size, x)
    if (options$split) {
        name = knitr:::fig_path(".tex", options)
        if (!file.exists(dirname(name)))
            dir.create(dirname(name))
        cat(x, file = name)
        sprintf("\\input{%s}", name)
}
else x 
}
)

blackAndWhite = TRUE
fastRlty = rep(1,20)
fastRlty = c(1,2,5,6,1,2,5,6,1,2,5,6)
trellis.par.set(theme=col.whitebg())
# trellis.par.set(theme=col.fastR(bw=blackAndWhite),lty=fastRlty)
trellis.par.set(theme=col.fastR())
# options(width=70)
options(continue="  ")
options(str = strOptions(strict.width = "wrap"))
options(show.signif.stars=FALSE)
options(digits=3)


# omit some of the output from summary( lm( ) )
print.summary.lm <- 
  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( stats:::print.summary.lm(x, digits=digits, symbolic.cor = symbolic.cor,
                              signif.stars=signif.stars, ...) )
    l <- sapply( output, nchar )
    w1 <- min( grep("Call", output) ) 
    w2 <- min( grep("Resid", output) ) 
    w3 <- min( grep("Coef", output) ) 
	rows <- 1:length(output)
	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
    cat( paste(output[keep], collapse="\n") )
    return(invisible(x))
  }

print.summary.glm <- 
  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( stats:::print.summary.glm(x, digits=digits, symbolic.cor = symbolic.cor,
                              signif.stars=signif.stars, ...) )
    l <- sapply( output, nchar )
    w1 <- min( grep("Call", output) ) 
    w2 <- min( grep("Resid", output) ) 
    w3 <- min( grep("Coef", output) ) 
	rows <- 1:length(output)
	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
    cat( paste(output[keep], collapse="\n") )
    return(invisible(x))
  }

## ----show_source, include=FALSE------------------------------------------
show_source <- 
  function(x, prompt=FALSE, highlight=TRUE, string.input=FALSE) {
  options = list(engine="R", prompt=prompt, highlight=highlight)
  if (! string.input) x <- deparse(substitute(x))
  paste0(
    "\\code{",
    knitr:::hilight_source(
      x, "latex", 
      options = options
    ),
    "}")
  }

## ----amsPreface, child="amsPreface.Rnw", eval=FALSE----------------------
## NA

## ----IntroChapter, child="IntroChapter.Rnw", eval=TRUE-------------------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Intro-")


## ----Data, child="Data.Rnw", eval=includeChapter[1]----------------------

## ----include=FALSE-------------------------------------------------------
set_parent("amsfast2.Rnw")
knitr::opts_chunk$set(cache = FALSE)

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Data-")

## ------------------------------------------------------------------------
require(fastR2)

## ------------------------------------------------------------------------
library(fastR2)

## ----iris-glimpse--------------------------------------------------------
require(fastR2)   # load fastR2 and its dependencies
glimpse(iris)     # glimpse lives in the dplyr package

## ----iris-head-----------------------------------------------------------
head(iris, n = 3)            # first three rows

## ----iris-tail-----------------------------------------------------------
tail(iris, n = 3)            # last three rows

## ----iris-subset---------------------------------------------------------
iris[50:51, 3:5]  # 2 rows and 3 columns

## ----iris-sample---------------------------------------------------------
sample(iris, 6)      # this requires mosaic::sample()

## ----eval=FALSE----------------------------------------------------------
## snippet("iris-glimpse")

## ----eval=FALSE----------------------------------------------------------
## snippet("iris-glimpse", exec = FALSE)

## ----iris-vector---------------------------------------------------------
iris$Sepal.Length    # get one variable and display as vector

## ----iris-vector2--------------------------------------------------------
iris$Species         # get one variable and display as vector

## ----iris-reload---------------------------------------------------------
iris <- "An iris is a beautiful flower."
str(iris)           # what is the structure of iris?
data(iris)          # explicitly reload the data set
str(iris)           # now it is a data frame again

## ----eval=FALSE----------------------------------------------------------
## ### Simpler version
## goal( ~ x, data = mydata)
## 
## ### Fancier version:
## goal(y ~ x | z, data = mydata)
## 
## ### Unified version:
## goal(formula, data = mydata)

## ----eval=FALSE----------------------------------------------------------
## Sepal.Length ~ Sepal.Width

## ----iris-scatter1, eval=FALSE-------------------------------------------
## xyplot(Sepal.Length ~ Sepal.Width, data = iris)

## ----iris-scatter2, eval=FALSE-------------------------------------------
## xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris)

## ----iris-scatter3, eval=FALSE-------------------------------------------
## xyplot(Sepal.Length ~ Sepal.Width, groups = Species,
##        data = iris)

## ----fig-irisDataIntro-xyplot, echo=FALSE--------------------------------


## ----irisDataIntro-xyplot23, echo=FALSE----------------------------------

xyplot(Sepal.Length ~ Sepal.Width, groups = Species, 
       data = iris, cex = 1.3, alpha = .8, auto.key = list(columns = 3))

## ----iris-tally----------------------------------------------------------
tally( ~ Species, data = iris)  # make a table of values

## ----iris-tally2---------------------------------------------------------
tally( ~ Sepal.Length, data = iris)  # make a table of values

## ----tally-logical-------------------------------------------------------
tally( ~ (Sepal.Length > 6.0), data = iris)  

## ----tally-cut-----------------------------------------------------------
tally( ~ cut(Sepal.Length, breaks = 2:10), data = iris)

## ----tally-cut2----------------------------------------------------------
tally( ~ cut(Sepal.Length, breaks = 2:10, right = FALSE), 
       data = iris)

## ----hist-v-hist, message=FALSE, fig.keep = "none"-----------------------
histogram( ~ Sepal.Length, data = iris)
hist(iris$Sepal.Length)
ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()

## ----hist-v-hist-fig, echo=FALSE, message=FALSE--------------------------
histogram( ~ Sepal.Length, data = iris)
hist(iris$Sepal.Length)
ggplot(iris, aes(x = Sepal.Length)) + geom_histogram()

## ----histogram-breaks, eval=FALSE----------------------------------------
## histogram( ~ Sepal.Length, data = iris,
##     breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10))

## ----histogram-condition, eval=FALSE-------------------------------------
## histogram(~ Sepal.Length | Species, data = iris)

## ----echo=FALSE----------------------------------------------------------
histogram( ~ Sepal.Length, data = iris, 
    breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10))
histogram(~ Sepal.Length | Species, data = iris)

## ----iris-histo-subset, eval=FALSE---------------------------------------
## histogram( ~ Sepal.Length | Species, data = iris,
##     subset = Species == "virginica")
## histogram( ~ Sepal.Length | Species,
##            data = filter(iris, Species == "virginica"))

## ----iris-histo-subset-fig, echo=FALSE-----------------------------------
histogram( ~ Sepal.Length | Species, data = iris,
    subset = Species == "virginica")
histogram( ~ Sepal.Length | Species, 
           data = filter(iris, Species == "virginica"))
histogram( ~ Sepal.Length | ntiles(Sepal.Width, 4, format = "interval"), data = iris)

## ----hist-freqp-fig, echo = FALSE----------------------------------------
histogram( ~ Sepal.Length, data = iris,  
           panel = function(x, ...) {
             panel.xhistogram(x, ..., col = "gray95")
             panel.freqpolygon(x, lwd = 3, ...)
           } )

## ----freqpolygon, fig.keep = "none"--------------------------------------
freqpolygon( ~ Sepal.Length, groups = Species, data = iris, 
             auto.key = TRUE)

## ----freqpolygon-fig, echo = FALSE---------------------------------------
freqpolygon( ~ Sepal.Length, groups = Species, data = iris, 
             auto.key = TRUE)

## ----echo=FALSE----------------------------------------------------------
set.seed(1)
a = rbinom(500, size = 15, p = 0.5)
a = c(a, 15-a)
c = c(
    rbinom(250, size = 15, p = 0.4), 
    rbinom(250, size = 15, p = 0.15), 
    rbinom(250, size = 15, p = 0.25), 
    rbinom(250, size = 15, p = 0.35)
    )
d = 15-c
b = c(rnorm(500, mean = 4, sd = 1.5), rnorm(500, mean = 7, sd = 1.5))

mydata = data.frame(
    x = c(a, c, d),
    dist = rep(c("symmetric", "pos. skewed", "neg. skewed"),
    each = 1000))

histogram( ~ x | dist, data = mydata, width = 1)
histogram(~eruptions, faithful, n = 20)

## ----faithful-histogram, eval=FALSE--------------------------------------
## histogram(~eruptions, faithful, n = 20)

## ----iris-mean-median----------------------------------------------------
mean( ~ Sepal.Length, data = iris) 
median( ~ Sepal.Length, data = iris )

## ----iris-mean-median-by-species-----------------------------------------
  mean(Sepal.Length ~ Species, data = iris)   
median(Sepal.Length ~ Species, data = iris)   

## ----faithful-mean-median------------------------------------------------
  mean( ~ eruptions, data = faithful)
median( ~ eruptions, data = faithful)

## ----faithful-stem-------------------------------------------------------
# stem does not understand the formula template
stem(faithful$eruptions)     

## ----pulse-histo-sol-----------------------------------------------------
histogram( ~ pulse, data = LittleSurvey)
histogram( ~ pulse, data = LittleSurvey, subset = pulse > 30) 
pulseSubset <- filter(LittleSurvey, pulse > 30)
mean(~ pulse, data = pulseSubset)
median(~ pulse, data = pulseSubset)

## ----number-prob-sol-----------------------------------------------------
t <- tally( ~ number, data = LittleSurvey); t
histogram(~number, LittleSurvey, width = 1)
max(t)
which(t == max(t))
which(t == min(t))
tally(~ (number %% 2 == 0), data = LittleSurvey)

## ----echo=FALSE, opts.label="fig1"---------------------------------------
a = rnorm(4000, mean = 10, sd = 2)
b = rnorm(4000, mean = 10, sd = 5)

mydata = data.frame(x = c(a, b), dist = rep(c("A", "B"), each = 4000))
histogram( ~ x | dist, mydata, n = 30, xlab = "", type = "density")

## ----intro-quantile------------------------------------------------------
quantile((1:10)^2)

## ----intro-quantile05a---------------------------------------------------
quantile((1:10)^2, type = 5)

## ----intro-quantile05b---------------------------------------------------
quantile((1:10)^2, type = 5, seq(0, 0.10, by = 0.005))

## ----faithful-quantile---------------------------------------------------
# note the different order of the arguments and 
# different output formats in these two functions.
quantile( ~ eruptions, data = faithful, probs = (0:10)/10)
qdata( ~ eruptions, (0:10)/10, data = faithful)
quantile( ~ eruptions, probs = (0:4)/4, data = faithful)
qdata( ~ eruptions, (0:4)/4, data = faithful)

## ----iris-bwplot, eval=FALSE---------------------------------------------
## bwplot(Sepal.Length~Species, data = iris)
## bwplot(Species~Sepal.Length, data = iris)
## bwplot(~eruptions, data = faithful)

## ----iris-bwplot-fig, echo=FALSE-----------------------------------------
bwplot(Sepal.Length~Species, data = iris)
bwplot(Species~Sepal.Length, data = iris)
bwplot(~eruptions, data = faithful)

## ----echo=FALSE----------------------------------------------------------
rescale <- function(x, lim = c(0, 10)) {
    return ( min(lim) + (x - min(x)) /
           (diff(range(x))) * (diff(range(lim))) )
}

n <- 400

a <- qnorm(ppoints(n))
a <- rescale(a)

b <- qexp(ppoints(n), 2)
b <- qbeta(ppoints(n), 3, 15)
b <- rescale(b)

c <- qbeta(ppoints(n), 20, 5)
c <- rescale(c)

d <- c(runif(n = n/2, min = 0, max = 10), qunif(ppoints(n/2), 0, 10) )
d <- rescale(d)

# bowl shaped
e <- 10 * c(rbeta(500, 6, 1), rbeta(500, 1, 6))
e <- c(qbeta(ppoints(100), 6, 1), qbeta(ppoints(100), 1, 6))
e <- rescale(e)

f <- c(0, 1, qbeta(ppoints(n-2), 12, 15))
f <- rescale(f)

y <- data.frame(A = a, B = b, C = c, D = d, E = e, F = f)

x <- stack(y)

z <- data.frame(W = a, Z = b, V = c, Y = d, U = e, X = f)

#z$W <- y$A
#z$Z <- y$B
#z$V <- y$C
#z$Y <- y$D
#z$U <- y$E
#z$X <- y$F

z <- stack(z)

histogram(~values|ind, data = x, xlab = "", as.table = TRUE,
    breaks = seq(-1, 11, by = 0.75),
    scales = list(alternating = FALSE, y = list(draw = F))
    )

levels(z$ind) <- rev(levels(z$ind))
bwplot(ind~values, data = z, range = 2.25, coef = 0, xlab = "", 
        ylab = "", as.table = TRUE)


## ------------------------------------------------------------------------
favstats(values~ind, data = x)
favstats(values~ind, data = z)

## ----fivenum-a-----------------------------------------------------------
fivenum(1:11) 
quantile(1:11) 

## ----fivenum-b-----------------------------------------------------------
fivenum(1:10) 
quantile(1:10) 

## ----bwplot-iqr-rule-sol-------------------------------------------------
x <- c(1:20, 20)
boxplot.stats(x)
iqr <- 16 - 6
# not an "outlier"
x <- c(1:20, 16 + 1.5 * iqr)
boxplot.stats(x)$out
# now it is an "outlier"
x <- c(1:20, 16 + 1.51 * iqr)
boxplot.stats(x)$out
###

## ----intro-dispersion02--------------------------------------------------
x <- c(1, 3, 5, 5, 6, 8, 9, 14, 14, 20)
n <- length(x); n
mean(x)
x - mean(x)
sum(x - mean(x))
abs(x - mean(x))
sum(abs(x - mean(x)))
mean(abs(x - mean(x)))
(x - mean(x))^2
sum((x - mean(x))^2)
sum((x - mean(x))^2) / (n-1)
var(x)
sd(x)
sd(x)^2

## ----dispersion-template-------------------------------------------------
mean(Sepal.Length ~ Species, data = iris)
var(Sepal.Length ~ Species, data = iris)
sd(Sepal.Length ~ Species, data = iris)
favstats(Sepal.Length ~ Species, data = iris)

## ----mad-----------------------------------------------------------------
mad(iris$Sepal.Length)

## ----mad-sol-------------------------------------------------------------
mad(iris$Sepal.Length)
median(abs(iris$Sepal.Length - median(iris$Sepal.Length))) * 1.4826

## ----pitching2005-era-sol------------------------------------------------
Pitching2 <- filter(Pitching2005, GS > 4)
favstats(ERA~lgID, data = Pitching2)
bwplot(lgID~ERA, data = Pitching2)
histogram(~ERA | lgID, data = Pitching2, layout = c(1, 2), width = .2)

## ----batting-ba-sol------------------------------------------------------
Batting2 <- droplevels(filter(Batting, AB >= 200))
Batting2 <- mutate(Batting2, BA = H / AB)
favstats(BA ~ league, data = Batting2)
bwplot(league ~ BA, data = Batting2)
histogram( ~ BA | league, data = Batting2, layout = c(1, 2))

## ----Batting-ba2-sol-----------------------------------------------------
bwplot( BA ~ factor(year) | league, data = Batting2)

## ----intro-deathPenalty01------------------------------------------------
tally(death ~ victim, data = DeathPenalty)

## ----intro-deathPenalty02------------------------------------------------
tally(death ~ defendant | victim, data = DeathPenalty)

## ----deathPenaltyMosaic-fig, echo=FALSE, message=FALSE-------------------
vcd::mosaic(~ victim + defendant + death, data = DeathPenalty)

## ----intro-deathPenalty03, fig.show="hide"-------------------------------
vcd::mosaic( ~ victim + defendant + death, data = DeathPenalty)
vcd::structable(~ victim + defendant + death, data = DeathPenalty)

## ----faithful-sol--------------------------------------------------------
xyplot(waiting ~ eruptions, data = faithful)
xyplot(head(waiting, -1) ~ tail(eruptions, -1), data = faithful)

## ----utilities-sol-------------------------------------------------------
xyplot(ccf ~ (year + month/12), data = Utilities, groups = month)
bwplot(ccf ~ factor(month), data = Utilities)

## ----utilities-ccfpday,  eval=FALSE--------------------------------------
## Utilities <- mutate(Utilities, ccfpday = ccf / billingDays)

## ----utilities2-sol------------------------------------------------------
Utilities <- mutate(Utilities, ccfpday = ccf / billingDays)
xyplot(ccfpday ~ (year + month/12), data = Utilities, groups = month)
bwplot(ccfpday ~ factor(month), data = Utilities)

## ----utilities-temp-sol--------------------------------------------------
xyplot(ccf ~ temp, data = Utilities)

## ----utilities-price-sol-------------------------------------------------
xyplot(ccf/gasbill ~ (12*year + month), data = Utilities)

## ----births-sol----------------------------------------------------------
xyplot(births~dayofyear, groups = dayofyear %% 7, data = Births78,
			auto.key = list(columns = 3))
bwplot(births~factor(dayofyear %% 7), groups = dayofyear %% 7, Births78)


## ----DiscreteDistribution, child="DiscreteDistributions.Rnw", eval=includeChapter[2]----

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Disc-")

## ----dice-sample-space-sol, tidy=FALSE-----------------------------------
Dice <- expand.grid(red = 1:6, blue = 1:6)
Dice %>% sample(4)
# part b 
Dice <-
  Dice %>% 
  mutate(
    A = red + blue >= 9,
    B = blue > red,
    C = blue == 5)

## ------------------------------------------------------------------------
Dice %>% filter(A)

## ------------------------------------------------------------------------
Dice %>% filter(B)

## ------------------------------------------------------------------------
Dice %>% filter(C)

## ------------------------------------------------------------------------
Dice %>% filter(A & B)

## ------------------------------------------------------------------------
Dice %>% filter( B | C)

## ------------------------------------------------------------------------
Dice %>% filter( A & (B | C))

## ----coin-toss, echo=FALSE, opts.label="fig1", seed=0--------------------
coinTosses <- data.frame(
  outcome = rbinom(1000, 1, 0.5),
  toss = 1:1000) %>% 
  mutate(relFreq = cumsum(outcome) / toss)
xyplot(relFreq ~ toss, coinTosses, type = "l",
        panel = function(x, y, ...){
            panel.abline(h = 0.5, lty = 1, col = "gray80")
            panel.xyplot(x, y, ...)
        },
#        main = "Results of 1000 simulated coin tosses",
        lwd = 2,
        ylim = c(0, 1),
        ylab = "relative frequency",
        xlab = "number of tosses")

## ----coin-toss-hist, echo=FALSE------------------------------------------
MoreTosses <- data.frame(heads = rbinom(1000, 1000, 0.5))
histogram( ~ heads / 1000, data = MoreTosses,
           main = "Results of 1000 simulations of 1000 coin tosses",
           xlim = c(0.44, 0.56),
           xlab = "proportion heads")
LotsMoreTosses <- data.frame(heads = rbinom(1000, 10000, 0.5))
histogram( ~ heads / 10000, data = LotsMoreTosses,
           main = "Results of 1000 simulations of 10,000 coin tosses",
           xlim = c(0.44,0.56),
           xlab = "proportion heads")

## ----print-sums-sol------------------------------------------------------
sums <- function(n){
    n <- n-3
    results <- character(0)
    for (x in 0:n) {
        for ( y in (0:(n-x)) ) {
            z <- n - x - y
            results <- c(results, 
                         paste(x + 1, "+", y+1, "+", z + 1, "=", x + y + z + 3))
        }
    }
    return(results)
}
length(sums(20))             # how many solutions?
sums(20)[1:10]               # first 10 solutions
sums(7)                      # smaller example

## ----choose--------------------------------------------------------------
choose(5, 2)

## ----choose02------------------------------------------------------------
4 * choose(13, 5) / choose(52, 5)

## ----cards-full-house, tidy=FALSE----------------------------------------
(choose(13, 1) *            # a number to have three of
 choose( 4, 3) *            # threee of that number
 choose(12, 1) *            # a different number to have two of
 choose( 4, 2)) /           # two of that numer
   choose(52, 5)

## ----cards-two-pair-sol, tidy=FALSE--------------------------------------
(choose(13, 2) *                   # two numbers (the pairs)
 choose( 4, 2) * choose(4, 2) *    # two suits for each pair
 choose(11, 1) * choose(4, 1)) /   # one more card of a different number
   choose(52, 5)

## ----cards-three-kind-sol, tidy=FALSE------------------------------------
(choose(13, 1) *                 # a number to have three of
 choose( 4, 3) *                 # three of that number
 choose(12, 2) *                 # two other numbers
 choose( 4, 1) * choose(4, 1)) / # one of each of those numbers
 choose(52, 5)

## ----number-of-suits-sol-------------------------------------------------
p<-rep(NA, 4)
p[1] <- choose(4, 1) *  choose(13, 5) / choose(52, 5)
p[2] <- choose(4, 2) * (choose(26, 5) - (choose(13, 5) +  choose(13, 5))) / 
           choose(52, 5) 
p[4] <- choose(4, 1) *  choose(13, 2) * 13 * 13 * 13 / choose(52, 5)
p[3] <- 1 - sum(p[-3])   # sum of all probabilities must be 1
rbind(1:4, p)

## ----birthday-problem-sol------------------------------------------------
# calculates prob of shared birthday
birthdayprob <- function(n) { 
        last <- 366 - n
        1 - ( prod(seq(last, 365)) / 365^n )
}

birthdayprob(10)
cbind(20:25, sapply(20:25, birthdayprob))

## ----birthday-sim-sol, cache=TRUE, seed=123------------------------------
birthdays69 <- rep(Births78$date, Births78$births)
sharedBirthday <- function(n, birthdays) {
	bdays <- sample( birthdays, n )
	length( unique(bdays) ) < n  
}
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays69)) 

# 1969
Births69 <- Births %>% filter(year == 1969)
birthdays69 <- rep(Births69$date, Births69$births)
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays69)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays69)) 

# 1988
Births88 <- Births %>% filter(year == 1988)
birthdays88 <- rep(Births88$date, Births88$births)
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(15, birthdays88)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(20, birthdays88)) 
tally( ~ sharedBirthday,  do(1000) *  sharedBirthday(25, birthdays88)) 

## ----donut-redo-sol------------------------------------------------------
choose(12 + 2, 2)

## ----sum-redo-sol--------------------------------------------------------
choose(17 + 2, 2)

## ----cards-same-color-sol------------------------------------------------
2 *                    # black or red
choose(26, 5) /        # 5 cards from that color
choose(52, 5)          # any 5 cards

## ----bayes-disease-sol---------------------------------------------------
p <- c(0.01, 0.10)
(p * 0.98) / (p * 0.98 + (1 - p) * 0.01)

## ----statistics-sol------------------------------------------------------
factorial(10) / (factorial(3) * factorial(3) * factorial(2))

## ----acceptance-sampling-sol---------------------------------------------
choose(90, 4) / choose(100, 4)  # prob only good ones selected
1 - choose(90, 4) / choose(100, 4)  # lot is rejected
f <- function(x) { 1 - choose(100 - x, 4) / choose(100, 4) }
xyplot(sapply(10:100, f) ~ 10:100, type = "l",
        xlab = "number of defective parts",
        ylab = "probability of rejecting",
        lwd = 2,
        col = "navy")

## ----acceptance-sampling-binomial----------------------------------------
dbinom(0, 4, 10/100)         # prob only good ones selected
1 - dbinom(0, 4, 10/100)     # lot is rejected

## ----acceptance-sampling-nbinom------------------------------------------
pnbinom(3, 1, 10/100)        # lot is rejected

## ----HHH-sol-------------------------------------------------------------
1/8 +  #    HHH--
1/16 + # or THHH-
1/16   # or -THHH

## ----HTH-sol-------------------------------------------------------------
1/8 +  #    HTH--
1/8 +  # or -HTH-
3/32   # or --HTH  but can't start HT

## ----HHT-sol-------------------------------------------------------------
1/8 +  #    HHT--
1/8 +  # or -HHT-
1/8    # or --HHT  

## ----mastermind----------------------------------------------------------
6^4                         # one of six colors in each of 4 holes
8^5                         # one of eight colors in each of 5 holes

## ----cards-flush-sol-----------------------------------------------------
choose(13 - 3, 2) /            # two of remaining 10 spades
choose(52 - 5, 2)              # two of 47 remaining cards in deck

## ----socks, tidy=FALSE---------------------------------------------------
8 * 5 * 4 / choose(17, 3)        # 1 sock of each kind means no pairs
1 - (8 * 5 * 4 / choose(17, 3))  # so this is prob of getting a pair

# or do it this way
( choose(8, 2) * 9 + choose(5, 2) * 12 + choose(4, 2) * 13 +
  choose(8, 3) + choose(5, 3) + choose(4, 3) ) / choose(17, 3)  

## ----probPlot, eval=FALSE------------------------------------------------
## # this will be wrong for values not among 0, 1, 2, 3, or 4
## f <- function(x) {
##   factorial(4) / (16 * factorial(x) * factorial(4 - x))
## }
## f(0:4)
## sum(f(0:4))    # check to be sure the probabilities add to 1
## xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability")
## xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability", type = "h")
## xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability", type = c("l", "p"))

## ----echo=FALSE,opts.label="fig3"----------------------------------------
# this will be wrong for values not among 0, 1, 2, 3, or 4
f <- function(x) {
  factorial(4) / (16 * factorial(x) * factorial(4 - x))
}
f(0:4)   
sum(f(0:4))    # check to be sure the probabilities add to 1
xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability")
xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability", type = "h")
xyplot(f(0:4) ~ 0:4, xlab = "x", ylab = "probability", type = c("l", "p"))

## ----echo=FALSE----------------------------------------------------------
plotDist("binom", params = list(size = 4, prob = 0.5), kind = "hist")
plotDist("binom", params = list(4, 0.5), kind = "cdf")

## ----binom-demo01, seed = 123--------------------------------------------
randomData <- rbinom(n = 30, size = 4, prob = 0.5)
randomData
tally( ~ randomData)
vals <- setNames(0:4, 0:4)             # add labels for nicer displays below
dbinom(vals, size = 4, prob = 0.5)     # matches earlier example 
dbinom(vals, size = 4, prob = 0.5) * 30  # pretty close to our table above
pbinom(vals, size = 4, prob = 0.5)       # same as cumsum(dbinom(...))

## ----plotDist------------------------------------------------------------
plotDist("binom", size = 4, prob = 0.5)
plotDist("binom", size = 4, prob = 0.5, kind = "cdf")
plotDist("binom", size = 4, prob = 0.5, kind = "histogram")

## ----freddy01------------------------------------------------------------
dbinom(20, 20, 0.8)            # probability of making all 20
1 - pbinom(14, 20, 0.8)        # probability of NOT making 14 or fewer
dbinom(16, 20, 0.8)            # probability of making exactly 16
plotDist("binom", size = 20, prob = 0.8)

## ----nbinom01------------------------------------------------------------
1 - pnbinom(c(18, 28, 38, 48), size = 1, prob = 1/36)

## ----deMere--------------------------------------------------------------
1 - dbinom(0, 4, 1/6)   # P(at least one 6 in 4 tries) 
pgeom(3, 1/6)           # P(fail at most 3 times before getting a 6)
1 - dbinom(0, 24, 1/36) # P(at least one double 6 in 24 tries)
pgeom(23, 1/36)         # P(fail at most 23 times before getting double 6)

## ----amy-----------------------------------------------------------------
1 - pnbinom(13, 100, 0.92)

## ----Hamming-sol, tidy = FALSE-------------------------------------------
dbinom(0, 4, 0.05)             # P(all four bits received correctly
pbinom(1, 7, 0.05)             # P(>= 6 of 7 bits received correctly)
p <- seq(0, 1, by = 0.01)
DD <- data.frame(
  probability = c(dbinom(0, 4, p), pbinom(1, 7, p)),
	error.rate = c(p, p),
	method = rep(c("plain", "Hamming"), each = length(p)))
xyplot(probability ~ error.rate, data = DD,
       groups = method, type = "l",
       xlab = "bitwise error rate", 
       ylab = "message error rate",
       auto.key = list(columns = 2, lines = TRUE, points = FALSE))

## ----freethrow04-sol-----------------------------------------------------
1 - pbinom(8, 10, 0.8)            # make at least 9 of 10
1 - pbinom(17, 20, 0.8)           # make at least 18 of 20

## ----freethrow02-sol-----------------------------------------------------
dbinom(10, 10, 0.8)       # make 10 straight

1 - pnbinom(4, 10, 0.80)  # at least 5 misses <-> at least 15 shots
pbinom(9, 14, 0.8)     # 9 or fewer makes in 14 tries <-> at least 15 shots
pbinom(10, 15, 0.8)    # 10 or fewer makes in 15 tries is INCORRECT

1 - pnbinom(4, 10, 0.70)  # at least 5 misses = at least 15 shots
pbinom(9, 14, 0.7)     # 9 or fewer makes in 14 tries <-> at least 15 shots
pbinom(10, 15, 0.7)    # 10 or fewer makes in 15 tries is INCORRECT

## ----freethrow03-sol-----------------------------------------------------
pp <- dbinom(5, 5, 0.80); pp  # a) prob make 5 straight (= success)
dgeom(1, pp)   # b) succeed with 1 miss (correct answer)
0.20 * pp      # miss first then make 5 straight (INCORRECT)
1 - pgeom(1, pp)             # c) miss more than one shot before success
probs <- dgeom(0:15, pp)     # d) 
#
xyplot(probs ~ 0:15, main = "Freddie",
                 xlab = "misses", ylab = "probability")
#################################################################
pp <- dbinom(5, 5, 0.70)    # a) prob make 5 straight (= success)
pp                             

dgeom(1, pp)     # b) succeed with 1 miss (correct answer)
0.20 * pp        # miss first then make 5 straight (_not_ the answer)

1 - pgeom(1, pp)           # c) miss more than one shot before success
probs <- dgeom(0:15, pp)   # d)
#
xyplot(probs ~ 0:15, main = "Frank",
       xlab = "misses", ylab = "probability")

## ----multiple-choice01-sol-----------------------------------------------
1 - pbinom(11, 20, 0.25)          # 11 or fewer correct fails
1 - pbinom(11, 20, 1/3)           # 11 or fewer correct fails
1 - pbinom(11, 20, 0.5 + 0.4 * 1/3 + 0.1 * 1/4)

## ----playoffs-part-------------------------------------------------------
### using binomial dist
1 - pbinom(1, 3, 0.6)              # win at least 2 of 3
### using neg binomial dist
pnbinom(1, 2, 0.6)                # lose <= 1 time  before 2 wins

## ----playoffs-sol--------------------------------------------------------
### using binomial dist
1- pbinom(1, 3, 0.6)              # win at least 2 of 3
1- pbinom(2, 5, 0.6)              # win at least 3 of 5
1- pbinom(3, 7, 0.6)              # win at least 4 of 7
### using neg binomial dist
pnbinom(1, 2, 0.6)                # lose <= 1 time  before 2 wins
pnbinom(2, 3, 0.6)                # lose <= 2 times before 3 wins
pnbinom(3, 4, 0.6)                # lose <= 3 times before 4 wins

## ----lady01--------------------------------------------------------------
1 - pbinom(8, 10, 0.5)
binom.test(9, 10)

## ----pval----------------------------------------------------------------
binom.test(9, 10) %>% pval()     # same as pval(binom.test(9, 10))

## ----lady02--------------------------------------------------------------
binom.test(9, 10, alternative = "greater")

## ----hugo01--------------------------------------------------------------
binom.test(16, 50, 1/6)

## ----hugo02--------------------------------------------------------------
# one-sided test manually and using binom.test()
1 - pbinom(15, 50, 1/6)
binom.test(16, 50, 1/6, alternative = "greater")

## ----hugo03, digits = 5--------------------------------------------------
# finding the "other side" by inspection:
dbinom(16, 50, 1/6)
data.frame(x = 0:4, `P(X=x)` = dbinom(0:4, 50, 1/6), check.names = FALSE)

# this should match the p-value from binom.test()
pbinom(1, 50, 1/6) + 1 - pbinom(15, 50, 1/6)
# letting R automate finding the interval too:
probs <- dbinom(0:50, 50, 1/6) 
sum(probs[probs <= dbinom(16, 50, 1/6)])

## ----spinner-sol---------------------------------------------------------
probs <- dbinom(0:50, 50, 0.25)
sum(probs[probs <= dbinom(8, 50, 0.25)])    # sum the small probs
binom.test(8, 50, 0.25)                     # check with binom.test()

## ----Gus-sol-------------------------------------------------------------
binom.test(8, 10, p = 0.50)

## ----mendel-sol----------------------------------------------------------
binom.test(428, 428 + 152, p = 0.75)           

## ----beetles-sol---------------------------------------------------------
binom.test(10, 10 + 17)
binom.test(36, 7 + 36)

## ----alpha-not-exact-sol-------------------------------------------------
data.frame(
  x = 7:9,
  `P(X <= x)` =  pbinom(7:9, 25, 0.5),
  check.names = FALSE
)

## ----power01-------------------------------------------------------------
qbinom(0.025, 100, 0.5)  # find q with pbinom(q, 100, 0.5) >= 0.025
pbinom(39:40, 100, 0.5)  # double checking

## ----power02-------------------------------------------------------------
pbinom(60, 100, 0.95) - pbinom(39, 100, 0.95)

## ----power03-------------------------------------------------------------
pbinom(60, 100, 0.55) - pbinom(39, 100, 0.55)

## ----power04, eval=FALSE-------------------------------------------------
## p <- seq(0, 1, by = 0.02)
## power <- 1 - (pbinom(60, 100, p) - pbinom(39, 100, p))
## xyplot(power ~ p, ylab = "power", xlab = expression(pi[a]),
##        type = "l", lwd = 2)

## ----echo=FALSE----------------------------------------------------------
p <- seq(0, 1, by = 0.02)
power <- 1 - (pbinom(60, 100, p) - pbinom(39, 100, p))
xyplot(power ~ p, ylab = "power", xlab = expression(pi[a]),
       type = "l", lwd = 2)

## ----power05-bug, eval=FALSE, include=FALSE------------------------------
## p <- rep(c(0.52, 0.55, 0.60), each = 2000)
## plab <- paste("alt prob =", as.character(p))
## n <- rep(1:2000, times = 3)
## critical <- qbinom(0.025, size = n, prob = p)
## power <- 1 - (pbinom(n - critical + 1, n, p) - pbinom(critical - 1, n, p))
## xyplot(power ~ n | plab,
##        ylab = "power", xlab = "number of coin tosses",
##        ylim = c(0, 1.1), type = "l", lwd = 2)

## ----power05, eval=FALSE-------------------------------------------------
## binom_power <- function(n, p_alt, alpha = 0.05, p_null = 0.50) {
##   critical_low <- qbinom(    alpha/2, size = n, prob = p_null) - 1
##   critical_hi  <- qbinom(1 - alpha/2, size = n, prob = p_null) + 1
##   pbinom(critical_low, n, p_alt) + 1 - pbinom(critical_hi - 1, n, p_alt)
## }
## 
## PowerData <-
##   expand.grid(n = seq(5, 10000, by = 5), p_alt = c(0.52, 0.55, 0.60)) %>%
##   mutate(
##     power = binom_power(n, p_alt),
##     plab = paste("alt prob =", as.character(p_alt))
##   )
## 
## xyplot(power ~ n | plab, data = PowerData,
##        ylab = "power", xlab = "number of coin tosses",
##        ylim = c(0, 1.1), type = "l", lwd = 2)

## ----echo=FALSE----------------------------------------------------------


## ----power-sol1----------------------------------------------------------
qbinom(0.975, 200, 0.5)
qbinom(0.025, 200, 0.5)
pbinom(85:86, 200, 0.5)
1 - pbinom(114:115, 200, 0.5)

## ----power-sol2, tidy = FALSE--------------------------------------------
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

## ----power-sol3----------------------------------------------------------
power(c(200, 400))

## ----power-sol4----------------------------------------------------------
# find sample size with 90% power
uniroot(function(size){ as.numeric(power(size)[1]) -0.90}, c(400, 5000))

## ----power-sim-sol-------------------------------------------------------
sim_pval <- function(n, alt = 0.55, null = 0.5) {
  pval(binom.test(rbinom(1, n, alt), n, prob = null))
}
Sims200 <- do(1000) * sim_pval(n = 200)
Sims400 <- do(1000) * sim_pval(n = 400)
Sims1075 <- do(1000) * sim_pval(n = 1075)
tally( ~ p.value < 0.05, data = Sims200, format = "proportion")
tally( ~ p.value < 0.05, data = Sims400, format = "proportion")
tally( ~ p.value < 0.05, data = Sims1075, format = "proportion")

## ----mean-coins01--------------------------------------------------------
vals <- 0:4
probs <- c(1, 4, 6, 4, 1) / 16     # providing probabilities directly
sum(vals * probs)
sum(0:4 * dbinom(0:4, 4, 0.5))     # using the fact that X is binomial

## ----mean-coins02--------------------------------------------------------
sum(0:100 * dnbinom(0:100, 3, 0.5))

## ----var01---------------------------------------------------------------
x <- 0:2
sum((x - 1)^2 * dbinom(x, 2, 0.5))   # same as above
n <- 5; p <- 0.2; x <- 0:n
sum((x - n*p)^2 * dbinom(x, n, p))     # X ~ Binom(5, 0.2)
n <- 5; p <- 0.8; x <- 0:n
sum((x - n*p)^2 * dbinom(x, n, p))     # X ~ Binom(5, 0.8)
n <- 10; p <- 0.8; x <- 0:n
sum((x - n*p)^2 * dbinom(x, n, p))     # X ~ Binom(10, 0.8)
n <- 20; p <- 0.8; x <- 0:n
sum((x - n*p)^2 * dbinom(x, n, p))     # X ~ Binom(20, 0.8)

## ----var02, digits = 4---------------------------------------------------
y <- 1:4
prob <- c(0.05, 0.20, 0.40, 0.35)
mean.y <- sum(y * prob); mean.y          # E(Y)
sum((y - mean.y)^2 * prob)               # Var(Y)
sum(y^2 * prob) - mean.y^2               # Var(Y) again

## ----Bernoulli-var-fig, echo = FALSE-------------------------------------
x <- seq(0, 1, by = 0.01)
xyplot( x * (1-x) ~ x, 
    lwd = 2, type = "l",
    main = "Variance of a Bernoulli random variable",
    xlab = expression(pi),
    ylab = expression(Var(X))
    )

## ----socks-exp-sol-------------------------------------------------------
vals <- 1:4
probs <- (4:1) / 10
sum(vals*probs)                             # expected value
sum(vals^2 * probs) - sum(vals * probs)^2   # variance 

## ----negbinomial-var-sol-------------------------------------------------
vals <- 0:50
probs.x <- dnbinom(vals, 3, 0.5)
probs.y <- dnbinom(vals, 3, 0.2)
var.x <- sum(vals^2 * probs.x) - sum(vals * probs.x)^2; var.x
var.y <- sum(vals^2 * probs.y) - sum(vals * probs.y)^2; var.y

# better approximation using more terms:
vals <- 0:500
probs.x <- dnbinom(vals, 3, 0.5)
probs.y <- dnbinom(vals, 3, 0.2)
var.x <- sum(vals^2 * probs.x) - sum(vals * probs.x)^2; var.x
var.y <- sum(vals^2 * probs.y) - sum(vals * probs.y)^2; var.y

## ----search-expval-------------------------------------------------------
# expected value of binary search
vals <- 1:8
probs <- 2^(vals - 1) / 255
sum(vals * probs)

# expected value of linear search
vals <- 1:255
probs <- rep(1/255, 255)
sum(vals * probs)

## ----roulette-sol--------------------------------------------------------
val <- c(-1, 1)
prob <- c(20/38, 18/38)
sum( val * prob)                       # expected value
sum( val^2 * prob) - sum(val * prob)^2 # variance

## ----willy-sol-----------------------------------------------------------
val <- 1/(1:6)
sum(val * 1/6)         # expected value
1/3.5

## ----alice-bob-sol-------------------------------------------------------
f <- function(prob) { 
        prob  + prob * (1 - prob)^3/(1 - (1 - prob)^2) 
}
g <- function(prob) { f(prob) - 0.50 }
uniroot(g, c(0.20, 0.5))$root          # when g = 0, f = 0.5

## ----poisson-customers01-------------------------------------------------
dpois(0, 2)

## ----poisson-customers02-------------------------------------------------
1- ppois(9, 6)

## ----show-fumbles, echo=FALSE--------------------------------------------
histogram( ~ week1, data = Fumbles)
plotDist("pois", lambda = mean( ~ week1, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

## ----fumbles-------------------------------------------------------------
m <- max(~week1, data = Fumbles)
tally( ~ factor(week1, levels = 0:m), data = Fumbles)
favstats( ~ week1, data = Fumbles)

## ----fumbles-tally, tidy = FALSE, include=FALSE--------------------------
m <- max( ~ week1, data = Fumbles)
xbar <- mean( ~ week1, data = Fumbles); xbar
Week1 <-
  data_frame(
    fumbles = 0:m,
    `observed count` = 
      as.vector(tally( ~ factor(week1, levels = 0:m), data = Fumbles)),
    `model count` = 120 * dpois(0:m, xbar)
  ) %>% 
  mutate(
    `observerd pct` = 100 * `observed count` / 120,
    `model pct` = 100 * `model count` / 120
  )
Week1

## ----results = "asis", echo=FALSE----------------------------------------
print(xtable::xtable(Week1), include.rownames = FALSE)

## ----fumbles-plot, echo=FALSE--------------------------------------------
histogram(~week1, data = Fumbles)
plotDist("pois", lambda = mean( ~ week1, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

## ----pois-hint-----------------------------------------------------------
dpois(0, 6/3)                         # 0 customers in 1/3 hour
dpois(2, 6/3)                         # 2 customers in 1/3 hour

## ----pois-sol1-----------------------------------------------------------
dpois(0, 6/3)                         # 0 customers in 1/3 hour
dpois(2, 6/3)                         # 2 customers in 1/3 hour
1-ppois(6, 6)                         # more than 6 in 1 hour
dpois(6, 6)                           # exactly 6 in 1 hour
ppois(5, 6)                           # less than 6 in 1 hour
ppois(30, 24) - ppois(19, 24)         # 20 to 30 customers in 4 hours

## ----hockey-goals-sol----------------------------------------------------
1 - ppois(43, 206/506 * 89)

## ----hockey-goals-sol2---------------------------------------------------
1 - ppois(41, 206/506 * 88)

## ----hockey-goals-sol3---------------------------------------------------
max(which(dpois(1:30, 206/506 * 89) <= dpois(44, 206/506 * 89)))
ppois(28, 206/506 * 89)
ppois(28, 206/506 * 89) + 1 - ppois(43, 206/506 * 89)

## ----fumbles-23-sol1-----------------------------------------------------
m <- max( ~ week2, data = Fumbles)
tally( ~ factor(week2, levels = 0:m), data = Fumbles)
favstats( ~ week2, data = Fumbles)
m <- max( ~ week3, data = Fumbles)
tally( ~ factor(week3, levels = 0:m), data = Fumbles)
favstats( ~ week3, data = Fumbles)

## ----fumbles-23-sol2, fig.keep = "last"----------------------------------
histogram(~ week2, data = Fumbles, width = 1)
plotDist("pois", lambda = mean( ~ week2, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

## ----fumbles-23-sol3, fig.keep = "last"----------------------------------
histogram(~ week3, data = Fumbles, width = 1)
plotDist("pois", lambda = mean( ~ week3, data = Fumbles), 
         add = TRUE, type = c("p", "l"))

## ----fumbles-all-sol, fig.keep="last"------------------------------------
Fumbles <- Fumbles %>% mutate(all = week1 + week2 + week3)
histogram(~ all, data = Fumbles, width = 1,  main= "All fumbles weeks 1-3")
plotDist("pois", lambda = mean( ~ all, data = Fumbles), add = TRUE)

## ----fumbles-simulated-sol,cache=TRUE, fig.keep="last", opts.label = "fig1"----
Sims <- data.frame(fumbles = rpois(120 * 8, 1.75), 
                   sample = rep(LETTERS[1:8], each = 120))
 favstats( ~ fumbles | sample, data = Sims)
histogram( ~ fumbles | sample, data = Sims, width = 1, as.table = TRUE)
plotDist("pois", lambda = 1.75, add = TRUE)

## ----youth-soccer--------------------------------------------------------
1 - phyper(3, m = 7, n = 5, k = 6) # from "girls' perspective"
phyper(2, m = 5, n = 7, k = 6)       # redone from "boys' perspective"

## ----lady-hyper----------------------------------------------------------
setNames(1 - phyper(-1:4, 5, 5, 5), paste0("x=", 0:5))

## ----lady-binom----------------------------------------------------------
setNames(1 - pbinom(2 * (0:5) - 1, 10, 0.5), paste0("x=", 2 * 0:5))

## ----fisher-twins01------------------------------------------------------
phyper(2, 17, 13, 12)
convictions <- rbind(dizygotic = c(2, 15), monozygotic = c(10, 3))
colnames(convictions) <- c("convicted", "not convicted")
convictions
fisher.test(convictions, alternative = "less")

## ----fisher-twins02------------------------------------------------------
fisher.test(convictions)

## ----ticket01-sol--------------------------------------------------------
d <- cbind(c(9, 13), c(14, 9)); d
fisher.test(d)
phyper(9, 23, 22, 22)

## ----ticket02-sol--------------------------------------------------------
d <- cbind(c(61, 103), c(69, 44)); d
fisher.test(d)
phyper(61, 61 + 103, 44 + 69, 61 + 69)

## ----ticket01-02-sol-----------------------------------------------------
or <- 9/13 / (14/9); c(or, 1/or)
or <- 61/103 / (69/44);  c(or, 1/or)

## ----fisher-twins1-alt-sol-----------------------------------------------
phyper(3, 13, 17, 18)
convictions <- rbind(monozygotic = c(3, 10), dizygotic = c(15, 2)) 
colnames(convictions) <- c("not convicted", "convicted")
convictions
fisher.test(convictions, alternative = "less")

## ----first-digit---------------------------------------------------------
firstDigit <- function(x) {
    trunc(x / 10^(floor(log10(abs(x)))))
}
# lengths (mi) of 141 major North American rivers
y0 <- log10(2:10) - log10(1:9)
y1 <- tally( ~ firstDigit(rivers), format = "prop")         # lengths in miles
y2 <- tally( ~ firstDigit(rivers * 1.61), format = "prop")  # lengths in km
xyplot(y0 + y1 + y2 ~ 1:9, auto.key = list(space = "right"), type = c("p", "l"))

## ----kings-and-queens-sol------------------------------------------------
numerator <-                  # P(K=2 & Q=2) 
    choose(4, 2) *            # pick 2 Kings
    choose(4, 2) *            # pick 2 Queens
    choose(52 - 4 - 4, 1) /   # pick 1 other card
    choose(52, 5)

denominator <-                # P(Q = 2)
    choose(4, 2) *            # pick 2 Queens
    choose(52 - 4, 3) /       # pick 3 other cards
    choose(52, 5)

numerator / denominator

## ----kings-and-hearts-sol------------------------------------------------
numerator <-                  # P(K=2 & H=2) 
(   1 *                       # pick King of Hearts
    choose( 3, 1) *           # pick one other King
    choose(12, 1) *           # pick one other heart
    choose(52 - 1 - 12 - 3, 2) +    # pick 2 other cards
#
    choose( 3, 2) *            # pick 2 Kings (not hearts)
    choose(12, 2) *            # pick 2 hearts (not King)
    choose(52 - 1 - 12 - 3, 1)      # pick 1 other card
) / choose(52, 5)

denominator <-               # P(H = 2)
    choose(13, 2) *          # pick 2 hearts
    choose(52 - 13, 3) /     # pick 3 other cards
    choose(52, 5)

numerator / denominator

## ----conditional-urn-sol-------------------------------------------------
# P(B=b | W=w) = dhyper(b, 3, 5, 3 - w)
probs <- 
  outer(0:2, 0:3, function(x, y){dhyper(y, 3, 5, 3 - x)}) 
colnames(probs) = paste("B=", 0:3, sep="")
rownames(probs) = paste("W=", 0:2, sep="")
probs
fractions(probs)
#
# P(R=r | W=w) = dhyper(r, 5, 3, 3-w)
probs <- 
  outer(0:2, 0:3, function(x, y){dhyper(y, 5, 3, 3 - x)})
colnames(probs) = paste("R=", 0:3, sep = "")
rownames(probs) = paste("W=", 0:2, sep = "")
probs
fractions(probs)


## ----ContinuousDistributions, child="ContinuousDistributions.Rnw", eval=includeChapter[3]----

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Cont-")

## ----pdfdef, include=FALSE-----------------------------------------------
p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
histogram( ~ x, n = 10, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE))
histogram( ~ x, n = 40, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE), 
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)
histogram( ~ x, n = 161, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE),
    par.settings = list(plot.polygon = list(border = "transparent")),
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)

## ----echo=FALSE, opts.label="fig3"---------------------------------------
p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
histogram( ~ x, n = 10, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE))
histogram( ~ x, n = 40, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE), 
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)
histogram( ~ x, n = 161, xlab = "", type = "density", ylab = "",
    scales = list(draw = FALSE),
    par.settings = list(plot.polygon = list(border = "transparent")),
	lattice.options = list( 
		plot.polygon = list(border = trellis.par.get("plot.polygon")$col)
	)
)

## ----pdf-example01fig, echo=FALSE, results="hide"------------------------
# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
plotFun(f(x) ~ x, xlim = c(-1, 4))

## ----pdf-example01-------------------------------------------------------
# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
plotFun(f(x) ~ x, xlim = c(-1, 4))

## ------------------------------------------------------------------------
integrate(function(x) x^2/9, 0, 3)

## ----dist01-sol----------------------------------------------------------
kernel <- function(x) { (x - 2) * (x + 2) * (x >= -2 & x <= 2) }
k <- 1 / integrate(kernel, -2, 2)$value; k
f <- function(x) { k * kernel(x) }
fractions(k)
integrate(f, -2, 2)                    # check that we have pdf
integrate(f,  0, 2)                    # P( 0 <= X <= 2 )
fractions(integrate(f, 0, 2)$value)
integrate(f, 1, 2)                     # P( 1 <= X <= 2 )
fractions(integrate(f, 1, 2)$value)
integrate(f, -1, 1)                    # P( -1 <= X <= 1 )
fractions(integrate(f, -1, 1)$value)

## ----dist02-sol----------------------------------------------------------
kernel <- function(x) { x * (x-3) * as.numeric(x >= 0 & x <= 3) }
k <- 1 / integrate(kernel, 0, 3)$value; k
fractions(k)
g <- function(x) { k * kernel(x) }
integrate(g, 0, 3)                    # check that we have pdf
integrate(g, 0, 1)
integrate(g, 0, 1) %>% value() %>% fractions()
integrate(g, 0, 2)
integrate(g, 0, 2) %>% value() %>% fractions()
integrate(g, 1, 2)
integrate(g, 1, 2) %>% value() %>% fractions()

## ----cauchy01-sol--------------------------------------------------------
tan(0.4 * pi)
qcauchy(0.9)

## ----cont-uniform--------------------------------------------------------
x <- -2:12; x
f <- function(x) { 0.1 * (0 <= x & x <= 10) }
f(x)   # sanity check 
# numerical integration gives approximation and tolerance
integrate(f, 7, 10)   
integrate(f, 3, 7)
integrate(f, 7, 15)

## ----uniform-pdf-cdf, echo=FALSE-----------------------------------------
plotFun(
  dunif(x) ~ x,
  xlim = c(-0.5, 1.5),
  ylim = c(-0.1, 1.2),
  main = "pdf for Unif(0,1)",
  xlab = "x",
  ylab = expression(f(x))
)

plotFun(
  punif(x) ~ x,
  xlim = c(-0.5, 1.5),
  ylim = c(-0.1, 1.2),
  main = "cdf for Unif(0,1)",
  xlab = "x",
  ylab = expression(F(x))
)

## ----runif---------------------------------------------------------------
runif(6, 0, 10)     # 6 random values on [0,10]
dunif(5, 0, 10)     # pdf is 1/10
punif(5, 0, 10)     # half the distribution is below 5
qunif(0.25, 0, 10)  # 1/4 of the distribution is below 2.5

## ----cdf-method01--------------------------------------------------------
g <- function(y) { 1 / (2 * sqrt(y)) * (0 <= y & y <= 1) }
integrate(g, 0, 1)

## ----unif-trans-check----------------------------------------------------
fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
plotFun(fV(v) ~ v, v.lim = c(-5, 5))

## ----exponential-pdf-cdf, echo=FALSE-------------------------------------
h <- 1.5
xyplot(c(0, h) ~ c(0, 5), 
	main = "Exponential pdfs",
	xlab = "",
	ylab = "Density",
	scales = list(axs = "i"),
	key = list(
		text = list(
			c(expression(paste(lambda, "=1")),
				expression(paste(lambda, "=0.5")),
				expression(paste(lambda, "=1.5"))
			),
			col = "gray30", cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dexp, args = list(rate = 1), n = 100, 
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dexp, args = list(rate = 0.5), n = 100, 
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dexp, args = list(rate = 1.5), n = 100, 
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)

mypexp <- function(x, ...){ pexp(q = x, ...) }

xyplot(c(0, h) ~ c(0, 5), 
	main = "Exponential cdfs",
	xlab = "",
	ylab = "Probability",
	scales = list(axs = "r"),
	key = list(
		text = list(
			c(expression(paste(lambda, "=1")),
				expression(paste(lambda, "=0.5")),
				expression(paste(lambda, "=1.5"))
			),
			col = "gray30", cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(mypexp, args = list(rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(mypexp, args = list(rate = 0.5), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(mypexp, args = list(rate = 1.5), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)

## ----exp-from-unif-sol, seed = 2357--------------------------------------
U <- runif(10000) 
X <- (-10 * log(U)) %>% sort()
Y1 <- rexp(10000, rate = 1/10) %>% sort()
Y2 <- rexp(10000, rate = 1/10) %>% sort()
xyplot(Y1 ~ X, panel = function(x, y, ...){
  panel.abline(0, 1)
  panel.xyplot(x, y, ...)
  })
xyplot(Y2 ~ Y1, panel = function(x, y, ...){
  panel.abline(0, 1)
  panel.xyplot(x, y, ...)
  })

## ----bank01--------------------------------------------------------------
1 - pexp(1/4, 10)    #   10 customers per  1 hour
1 - pexp(15, 1/6)    #  1/6 customer  per  1 minute
dpois(0, 10/4)       # 10/4 customers per 15 minutes

## ----pois-sim, eval=FALSE, seed=123, tidy = FALSE------------------------
## PoisSim <-
##   expand.grid(run = 1:10, i = 1:40) %>%
##   group_by(run) %>%
##   mutate(interval = rexp(40), time = cumsum(interval))
## stop <- min(max(time ~ run, data = PoisSim))  # shortest run?
## stop <- 5 * trunc(stop / 5)                   # truncate to multiple of 5
## stripplot(run ~ time, data = PoisSim %>% filter(time <= stop),
##           pch = 1, cex = .7, col = "black",
## 	panel = function(x, y, ...){
## 		panel.abline(h = seq(1.5, 9.5, by = 1), col = "gray60")
## 		panel.abline(v = seq(0, stop, by = 5), col = "gray60")
## 		panel.stripplot(x, y, ...) })

## ----echo=FALSE, opts.label="fig1", cache=FALSE, seed=123----------------
PoisSim <- 
  expand.grid(run = 1:10, i = 1:40) %>%
  group_by(run) %>%
  mutate(interval = rexp(40), time = cumsum(interval)) 
stop <- min(max(time ~ run, data = PoisSim))  # shortest run? 
stop <- 5 * trunc(stop / 5)                   # truncate to multiple of 5 
stripplot(run ~ time, data = PoisSim %>% filter(time <= stop), 
          pch = 1, cex = .7, col = "black",
	panel = function(x, y, ...){
		panel.abline(h = seq(1.5, 9.5, by = 1), col = "gray60") 
		panel.abline(v = seq(0, stop, by = 5), col = "gray60") 
		panel.stripplot(x, y, ...) })

## ----fumbles02-----------------------------------------------------------
1 - pexp(0.5, rate = 1.75)
dpois(0, 1.75 / 2)

## ----prob-cdf01, tidy = FALSE--------------------------------------------
f <- function(x) { x/2 }                   # define pdf
integrate(f, lower = 0, upper = 2)         # check it is a pdf
xf <- function(x) { x * f(x) }                    
integrate(xf, lower = 0, upper = 2)        # expected value
xxf <- function(x) { x^2 * f(x) }
integrate(xxf, lower = 0, upper = 2)       # E(X^2)

# compute the variance using E(X^2) - E(X)^2
integrate(xxf, lower = 0, upper = 2)$value  - 
  (integrate(xf, lower = 0, upper = 2)$value)^2    

## ------------------------------------------------------------------------
g <- makeFun(3 * x^2 / 8 ~ x)
integrate(function(x) x * g(x), 0, 2)

## ----tidy = FALSE--------------------------------------------------------
integrate(function(x){ x^2 * g(x) }, 0, 2)
integrate(function(x){ x^2 * g(x) }, 0, 2)$value -
  (integrate(function(x){ x * g(x) }, 0, 2)$value)^2

## ----prob-cdf02-sol, tidy = FALSE----------------------------------------
g <- function(x) { 3 * x^2 / 8 }       # define pdf
integrate(f, lower = 0, upper = 2)     # check it is a pdf
xg <- function(x) { x * g(x) }
integrate(xg, lower = 0, upper = 2)    # expected value
xxg <- function(x) { x^2 * g(x) }
integrate(xxg, lower = 0, upper = 2)   # E(X^2)

# compute the variance using E(X^2) - E(X)^2
integrate(xxg, lower = 0, upper = 2)$value  - 
  (integrate(xg, lower = 0, upper = 2)$value)^2    

## ----prob-traffic01-sol--------------------------------------------------
a <- 1
f <- function(x) {1 / x^4}
k <- 1 / integrate(f, a, Inf)$value; k
f <- function(x) {k / x^4}
integrate(f, a, Inf)
integrate(f, 2, 3)

## ----prob-traffic02-sol--------------------------------------------------
# find the median
g <- function(x) { integrate(f, a, x)$value - 0.5 }
uniroot(g, c(1, 10))$root

## ----prob-traffic03-sol, tidy = FALSE------------------------------------
xf <- function(x) { x * f(x) }
Ex <- integrate(xf, a, Inf)$value;  Ex       # E(X)
xxf <- function(x) { x^2 * f(x) }
Exx <- integrate(xxf, a, Inf)$value;  Exx    # E(X^2)
Exx - (Ex)^2                                 # variance
sqrt(Exx - (Ex)^2)                           # st dev

## ----echo=FALSE----------------------------------------------------------
h <- dnorm(0, 0, 3/4) * 1.05
xyplot(c(0, h) ~ c(-3, 3), 
	main = expression(paste("Normal pdfs (", mu, "=0)")),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			c( expression(paste(sigma, "=0.7")),
			   expression(paste(sigma, "=1.0")),
			   expression(paste(sigma, "=1.5"))
			),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[c(2, 1, 3)],
			lty = 1,
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.97, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dnorm, args = list(mean = 0, sd = 1),  n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dnorm, args = list(mean = 0, sd = 0.70),  n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dnorm, args = list(mean = 0, sd = 1.5),  n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty= 1)
	}
	)

## ----normal01------------------------------------------------------------
# these two should return the same value:
pnorm(5, mean = 3, sd = 2)    # 5 is 1 st dev above the mean of 3
pnorm(1)                  

## ----empirical-rule------------------------------------------------------
pnorm(1:3) - pnorm(-1:-3)

## ----normal-sat----------------------------------------------------------
pnorm(700, 500, 100) - pnorm(400, 500, 100)
1- pnorm(800, 500, 100)

## ----sat-rescale---------------------------------------------------------
pnorm(500, 422, 100)
pnorm(500, 475, 100)

## ----sat-percentile------------------------------------------------------
qnorm(0.80, 500, 100)
qnorm(0.80, 500, 110)

## ------------------------------------------------------------------------
1 - pnorm(800, 500, 110)

## ----gamma00, echo=FALSE-------------------------------------------------
require(grid)
inputs <- seq(0.05, 5, by = 0.05)
xyplot(gamma(inputs) ~ inputs, type = "l",
        ylim = c(0, factorial(4)),
        xlab = "x",
        ylab = expression(Gamma(x)),
        panel = function(x, y, ...){ 
            panel.xyplot(x, y, ...)
            grid.points(1:5, factorial(0:4), gp = gpar(pch = 16, cex = 0.5))
        }
    )

## ----gamma-pdfs, echo=FALSE----------------------------------------------
h <- dgamma(0, 1, 1) * 1.05
dtemp <- function(x, mean = 0, sd = 1, rate = 1, shape = 1) {
  dgamma(x, shape = shape, rate = rate)
}
xyplot(c(0, h) ~ c(0, 4), 
	main = "Gamma pdfs (rate=1)",
	xlim = c(0, 4),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("shape = ", c(1, 2, 3), sep = ""),
			col = "gray30"
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1,
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape = 1, rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 3, rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)
xyplot(c(0, h) ~ c(0, 4), 
	main = "Gamma pdfs (shape=2)",
	xlim = c(0, 4),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("rate=", c(0.5, 1.0, 2.0), sep = ""),
			col = "gray30"
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[c(2, 1, 3)],
			lty = 1,
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.95, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 0.5), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1)
		panel.mathdensity(dmath = dtemp, args = list(shape = 2, rate = 2), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1)
	}
	)

## ----weibull-pdfs, echo=FALSE--------------------------------------------
h <- 1.6
dtemp <- function(x, mean = 0, sd = 1, scale = 1, shape = 1) {
  dweibull(x, scale = scale, shape = shape)
}

xyplot(c(0, h) ~ c(0, 3), 
	main = "Weibull pdfs (shape=1)",
	xlim = c(0, 2.2),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("scale = ", 1:3, sep = ""),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.97, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 1),  n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2, lty = 1) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 2, shape = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2, lty = 1) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 3, shape = 1), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2, lty = 1) 
	}
	)

xyplot(c(0, h) ~ c(0, 3), 
	main = "Weibull pdfs (scale=1)",
	xlim = c(0, 2.2),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste("shape = ", 1:3, sep = ""),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = trellis.par.get("superpose.line")$col[1:3],
			lty = 1, 
			lwd = 2
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(1, 1),
		x = 0.97, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp,  args = list(scale = 1, shape = 1),  n = 100,
			col = trellis.par.get("superpose.line")$col[1], lwd = 2) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 2), n = 100,
			col = trellis.par.get("superpose.line")$col[2], lwd = 2) 
		panel.mathdensity(dmath = dtemp, args = list(scale = 1, shape = 0.5), n = 100,
			col = trellis.par.get("superpose.line")$col[3], lwd = 2) 
	}
	)

## ----sol-middle-of-dist--------------------------------------------------
pexp(1+1) - pexp(1-1)
pexp(1/2 + 1/2, rate = 2) - pexp(1/2 - 1/2, rate = 2)
punif(0.5 + sqrt(1/12), 0, 1) - punif(0.5 - sqrt(1/12), 0, 1)
s <- sqrt(8 / (6^2*7))
pbeta(2/6 + s, shape1 = 2, shape2 = 4) - pbeta(2/6 - s, shape1 = 2, shape2 = 4)

## ----weibull01-sol-------------------------------------------------------
# a = shape; b = scale (acts like 1/lambda from exponential dist)

a <- 2; b <- 3
m <- b * gamma(1 + 1/a); m                                # mean
v <- b^2 * ( gamma(1 + 2/a) - gamma(1+1/a)^2 ); v         # var
qweibull(0.5, a, b)                                       # median
pweibull(m, a, b)                                         # less than mean
pweibull(6, a, b) - pweibull(1.5, a, b)                   # in a range
pweibull(m+sqrt(v), a, b) - pweibull(m - sqrt(v) , a, b)  # near mean

# with roles of parameters reversed 

a <- 3; b <- 2
m <- b * gamma(1 + 1/a); m                                # mean
v <- b^2 * ( gamma(1 + 2/a) - gamma(1+1/a)^2 ); v         # var
qweibull(0.5, a, b)                                       # median
pweibull(m, a, b)                                         # less than mean
pweibull(6, a, b) - pweibull(1.5, a, b)                   # in a range
pweibull(m+sqrt(v), a, b) - pweibull(m - sqrt(v), a, b)   # near mean


## ----beta-01-sol---------------------------------------------------------
m <- 5/(2+5); m                                       # mean
v <- 5 * 2 / ( (5+2)^2 * (5 + 2 + 1) ); v             # var
qbeta(0.5, 5, 2)                                      # median
pbeta(m, 5, 2)                                        # less than mean
pbeta(0.4, 5, 2) - pbeta(0.2, 5, 2)                   # in a range
pbeta(m+sqrt(v), 5, 2) - pbeta(m-sqrt(v), 5, 2)       # near mean

## ----beta-pdfs, echo=FALSE-----------------------------------------------
h <- 4
dtemp <- function(x, mean = 0, sd = 1, shape1 = 1, shape2 = 1) {
  dbeta(x, shape1, shape2)
}

mylty <- c(1, 6, 6, 2, 2)
mycol <- trellis.par.get("superpose.line")$col[c(1, 2, 3, 2, 3)]
xyplot(c(0, h) ~ c(0, 1), 
	main = "Beta pdfs",
	xlim = c(0, 1),
	ylim = c(0, 5),
	xlab = "",
	ylab = "Density",
	key = list(
		text = list(
			paste0(  "shape1=", c(0.5, 2.0, 5.0, 2.0, 0.5), 
				   ", shape2=", c(0.5, 5.0, 2.0, 0.5, 2.0)),
			col = "gray30",
			cex = 0.7
		),
		lines = list(
			col = mycol,
			lty = mylty,
			lwd = c(2, 2, 2, 2, 2)
			),
		border = TRUE,
		padding.text = 1.5,
		corner = c(0.5, 1),
		x = 0.5, y = 0.95
		),
	panel = function(x, y, ...) {
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 0.5, shape2 = 0.5), n = 100,
			col = mycol[1], lwd = 2, lty = mylty[1])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 2, shape2 = 5), n = 100,
			col = mycol[2], lwd = 2, lty = mylty[2])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 5, shape2 = 2), n = 100,
			col = mycol[3], lwd = 2, lty = mylty[3])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 2, shape2 = 0.5), n = 100,
			col = mycol[4], lwd = 2, lty = mylty[4])
		panel.mathdensity(dmath = dtemp, args = list(shape1 = 0.5, shape2 = 2), n = 100,
			col = mycol[5], lwd = 2, lty = mylty[5])
	}
	)

## ----faithful-hist-density, echo=FALSE-----------------------------------
times <- faithful$eruptions
histogram( ~ times, type = "density", breaks = seq(1, 6, by = 0.5))
densityplot( ~ times)

## ----kde01-data----------------------------------------------------------
x <- c(2.2, 3.3, 5.1, 5.5, 5.7, 5.7, 6.9, 7.8, 8.4, 9.6)

## ----kde01---------------------------------------------------------------
K1 <- function(x) { # rectangular
    return( as.numeric( -1 < x & x < 1 ) )
}
K2 <- function(x) { # triangular
    return( (1 - abs(x)) * as.numeric(abs(x) < 1) )
}
K3 <- function(x) { # parabola / Epanechnikov
     return( (1 - x^2) * as.numeric(abs(x) < 1) )
}
K4 <- dnorm         # Gaussian

## ----faithful-kde, echo=FALSE--------------------------------------------
# times <- faithful$eruptions
# densityplot( ~ times, kernel = "rectangular",
#     main = "Rectangular kernel")
# densityplot( ~ times, kernel = "triangular",
#     main = "Triangular kernel")
# densityplot( ~ times,
#     main = "Normal kernel")
# densityplot( ~ times, adjust = 0.25,
#     main = "Normal kernel; adjust=0.25")
# density(times)       # display some information about the kde

plotFun(K1(x) ~ x, xlim = c(-3, 3), main = expression(K[1])) 
plotFun(K2(x) ~ x, xlim = c(-3, 3), main = expression(K[2]),
        discontinuity = Inf)
plotFun(K3(x) ~ x, xlim = c(-3, 3), main = expression(K[3]),
        discontinuity = Inf)
plotFun(K4(x) ~ x, xlim = c(-3, 3), main = expression(K[4]),
        discontinuity = Inf)

## ----kde02---------------------------------------------------------------
kde <- function(data, kernel = K1, ...) {
    n <- length(data)
    scalingConstant = integrate(function(x){kernel(x, ...)}, -Inf, Inf)$value
    function(x) {
        mat <- outer(x, data, FUN = function(x, data) {kernel(x - data, ...)} )
        val <- rowSums(mat)
        val <- val / (n * scalingConstant)
        return(val)
    }
}

## ----kdeplot, echo=FALSE-------------------------------------------------
kdeplot <- function(data, kernel, xlim, ylim,
    args = list(),
    lty = 1,
    lwd = 3,
    col = trellis.par.get("plot.line")$col,
    buffer = 0,
    n = 50,
    xlab = "x",
    ylab = "density",
    ...) {

    ndata <- length(data)
    scalingConstant = integrate( function(x) { 
        do.call( kernel, c(list(x), args) ) }, 
        -Inf, Inf)$value

    if (missing(xlim)) {
        xlim = range(data)
        buf <- buffer * diff(xlim)
        xlim <- xlim + c(-1, 1) * buf
    }

    if (missing(ylim)) {
        xvals <- seq(xlim[1], xlim[2], length = n)
        # yvals <- fun(xvals, unlist(args))
        yvals = do.call(kernel,
                      c(list(x = seq(xlim[1], xlim[2], length = 100)), args))
        ylim <- range(yvals)
        buf <- buffer * diff(ylim)
        ylim <- ylim + c(-1, 1) * buf
    }

    xyplot(ylim ~ xlim, xlab = xlab, ylab = ylab,
            panel = function(x, y, ...){ 
            panel.mathdensity(kde(data, kernel, ...),
                args = args,
                lwd = 3.5,
                lty = lty,
                col = col,
                n = n,
                ...) 
            for (d in data) {
            panel.mathdensity(
                dmath = function(x){
                    y <- do.call(kernel, c(list(x-d), args))
                    y / (ndata * scalingConstant)
                },
                args = args,
                lwd = 1.5,
                lty = 1,
                col = trellis.par.get("superpose.line")$col[2],
                n = n,
                ...) 
            }
            panel.rug(data, lwd = 2)
            },
            ...)
}
 
kdeplot(x, K1, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[1]))
kdeplot(x, K2, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[2]))
kdeplot(x, K3, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[3]))
kdeplot(x, K4, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[4]))

## ----K5, echo=FALSE------------------------------------------------------
K5 <- function(x, ...) { dnorm(x, sd = sqrt(1/6), ...) }
kdeplot(x, K2, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[2]))
kdeplot(x, K5, xlim = c(1, 11), ylim = c(0, 0.35), n = 500,
    main = expression(K[5]))

## ----faithful-kde-again, echo=FALSE--------------------------------------
times <- faithful$eruptions
densityplot( ~ times, kernel = "rectangular",
    main = "Rectangular kernel")
densityplot( ~ times, kernel = "triangular",
    main = "Triangular kernel")
densityplot( ~ times,
    main = "Normal kernel")
densityplot( ~ times, adjust = 0.25,
    main = "Normal kernel; adjust=0.25")
#density(times)       # display some information about the kde

## ----faithful-kde-yet-again----------------------------------------------
times <- faithful$eruptions
kdeFaithfulRect <- densityplot( ~ times, kernel = "rectangular",
    main = "Rectangular kernel")
kdeFaithfulTri <- densityplot( ~ times, kernel = "triangular",
    main = "Triangular kernel")
kdeFaithfulNormal <- densityplot( ~ times,
    main = "Normal kernel")
kdeFaithfulNormal2 <- densityplot( ~ times, adjust = 0.25,
    main = "Normal kernel; adjust = 0.25")
density(times)       # display some information about the kde

## ----eval = FALSE--------------------------------------------------------
## sampleData <- do.call(rdist, args = c(list(n = size), args));

## ----ise-sol-------------------------------------------------------------
# Here is a simple way to estimate ise
ise <- function(density, distr, ...) {
  x <- density$x
  y <- density$y
  diffs <- diff(c(min(x), x, max(x)))
  dx <- .5 * (head(diffs, -1) + tail(diffs, -1))
  sum((y - distr(x, ...))^2 * dx)
}

# some sanity checks
x <- rnorm(100)
d <- density(x)
ise( d, dnorm )

## ----mise-sol, cache=TRUE------------------------------------------------
mise <- function(size = 20, reps = 100, dist = "norm", args = list(), ...) {
  results <- do(reps) * {
    data <- do.call(paste0("r", dist), c(list(n = size), args))
    distr <- function(x) { do.call(paste0("d", dist), c(list(x), args)) }
    d <- density(data, ...)
    data.frame(ise = ise(d, distr))
  }
  return(c(mise = mean( ~ ise, data = results)))
}

## ----mise-sims-sol, cache = TRUE, seed = 1234----------------------------
settings <- expand.grid( kernel = c("gaussian", "triangular",
                                  "rectangular", "epanechinikov"),
                         size = c(10, 30, 100),
                         adjust = c(1/3, 1, 3)
)
results <- 
  bind_rows(
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "normal", 
        mise = mise(size = size, reps = 500)),
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "exp", 
        mise = mise(size = size, reps = 500, dist = "exp")),
    settings %>% group_by(kernel, size, adjust) %>%
      summarise(
        dist = "beta", 
        mise = mise(size = size, reps = 500, dist = "beta", 
                    args = list(shape1 = 0.5, shape2 = 0.5)))
    )

## ----mise-plot-sol, opts.label = "figbig"--------------------------------
ggplot(aes(y = mise, x = adj, colour = kernel, group = kernel), 
       data = results %>% mutate(adj = factor(round(adjust, 2)))) +
  geom_line(alpha = 0.7) +
  facet_grid( dist ~ size, scales = "free_y")       

## ----mise-beta-sol-------------------------------------------------------
densityplot( ~ rbeta(100, .5, .5) | "adjust=1", 
             ylim = c(0, 5), xlim = c(-0.5, 1.5))
plotDist("beta", shape1 = 0.5, shape2 = 0.5, under = TRUE, col = "gray50")
densityplot( ~ rbeta(100, .5, .5) | "adjust=1/3", 
             adjust = 1/3, ylim = c(0, 5), xlim = c(-0.5, 1.5))
plotDist("beta", shape1 = 0.5, shape2 = 0.5, under = TRUE, col = "gray50")
densityplot( ~ rbeta(100, .5, .5) | "adjust=3", 
             adjust = 3, ylim = c(0, 5), xlim = c(-0.5, 1.5))
plotDist("beta", shape1 = 0.5, shape2 = 0.5, under = TRUE, col = "gray50")
densityplot( ~ rbeta(100, .5, .5) | "adjust=0.1", 
             adjust = 0.1, ylim = c(0, 5), xlim = c(-0.5, 1.5))
plotDist("beta", shape1 = 0.5, shape2 = 0.5, under = TRUE, col = "gray50")

## ----normal-quantile01, fig.keep="none"----------------------------------
x <- c(-0.16, 1.17, -0.43, -0.02, 1.06, 
       -1.35, 0.65, -1.12, 0.03, -1.44)
# sort the data
x.sorted <- sort(x); x.sorted
p <- seq(0.05, 0.95, by = 0.1); p
q <- qnorm(p); q
xyplot(x.sorted ~ q)

## ----normal-quantile01-fig, echo=FALSE, results = "hide"-----------------
x <- c(-0.16, 1.17, -0.43, -0.02, 1.06, 
       -1.35, 0.65, -1.12, 0.03, -1.44)
# sort the data
x.sorted <- sort(x); x.sorted
p <- seq(0.05, 0.95, by = 0.1); p
q <- qnorm(p); q
xyplot(x.sorted ~ q)
qqmath(x)               # generate the normal-quantile plot

## ----normal-quantile02, fig.show="hide"----------------------------------
ppoints(10)             # percentages for 10 data values
qqmath(x)               # generate the normal-quantile plot

## ----normal-quantile03a, echo=FALSE, opts.label="fig1", seed=123---------
dat10 <- 
  data.frame(
    x = rnorm(8*10),                # 8 samples of size 10
    size = rep(10, 8*10),           # record sample size
    sample = rep(1:8, each = 10)    # record sample number
  )
dat25 <- 
  data.frame(
    x = rnorm(8*25),                # 8 samples of size 25
    size = rep(25, 8*25),           # record sample size
    sample = rep(1:8, each = 25)    # record sample number
  )
dat100 <- 
  data.frame(
    x = rnorm(8*100),               # 8 samples of size 100
    size = rep(100, 8*100),         # record sample size
    sample = rep(1:8, each = 100)   # record sample number
  )
simdata <- rbind(dat10, dat25, dat100)

# generate the normal-quantile plots for each of the 30 samples
latticeExtra::useOuterStrips(
qqmath( ~ x | factor(sample) * factor(size), data = simdata,
    layout = c(8, 3), as.table = TRUE, cex = 0.5, 
    scales = list(relation = "free", draw = FALSE),
	xlab = "", ylab = "")     
)

## ----normal-quantile03, fig.keep = "none"--------------------------------
dat10 <- 
  data.frame(
    x = rnorm(8*10),                # 8 samples of size 10
    size = rep(10, 8*10),           # record sample size
    sample = rep(1:8, each = 10)    # record sample number
  )
dat25 <- 
  data.frame(
    x = rnorm(8*25),                # 8 samples of size 25
    size = rep(25, 8*25),           # record sample size
    sample = rep(1:8, each = 25)    # record sample number
  )
dat100 <- 
  data.frame(
    x = rnorm(8*100),               # 8 samples of size 100
    size = rep(100, 8*100),         # record sample size
    sample = rep(1:8, each = 100)   # record sample number
  )
simdata <- rbind(dat10, dat25, dat100)

# generate the normal-quantile plots for each of the 30 samples
latticeExtra::useOuterStrips(
qqmath( ~ x | factor(sample) * factor(size), data = simdata,
    layout = c(8, 3), as.table = TRUE, cex = 0.5, 
    scales = list(relation = "free", draw = FALSE),
	xlab = "", ylab = "")     
)

## ----normal-quantile04, seed=123, eval = FALSE---------------------------
## # sample of size 40 from Binom(50, 0.4)
## x <- rbinom(40, 50, 0.4); x
## xqqmath( ~ x, fitline = TRUE)

## ----normal-quantile04-fig, seed=123, echo = FALSE, results = "hide"-----
# sample of size 40 from Binom(50, 0.4)
x <- rbinom(40, 50, 0.4); x         
xqqmath( ~ x, fitline = TRUE)

## ----qq-weibull01, eval=FALSE--------------------------------------------
## life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
## qweib <- function(x) { qweibull(x, 1.2, 146) }
## xqqmath( ~ life01, distribution = qweib, idline = TRUE, qqmathline = FALSE)

## ------------------------------------------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
qweib <- function(x) { qweibull(x, 1.2, 146) } 
xqqmath( ~ life01, distribution = qweib, idline = TRUE, qqmathline = FALSE)

## ----qq-exp01, eval=FALSE------------------------------------------------
## life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
## mean(life01); 1 / mean(life01)
## qe <- function(x) { qexp(x, 1/mean(life01)) }
## xqqmath( ~ life01, distribution = qe, idline = TRUE, qqmathline = FALSE)
## xqqmath( ~ life01, distribution = qexp, qqmathline = FALSE)

## ----qq-exp01-fig--------------------------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
mean(life01); 1 / mean(life01)
qe <- function(x) { qexp(x, 1/mean(life01)) } 
xqqmath( ~ life01, distribution = qe, idline = TRUE, qqmathline = FALSE)
xqqmath( ~ life01, distribution = qexp, qqmathline = FALSE)

## ----qq-departures, echo=FALSE, seed=123, opts.label="fig1"--------------
dd <- rcauchy(50)
qqdata <- data.frame( 
	x = c(runif(100), rexp(100), rchisq(100, df = 2), dd, jitter(-dd)),
	dist = rep(c("A", "B", "C", "D"), each = 100) 
	)
xqqmath( ~ x | dist, data = qqdata,
		scales = list(relation = "free", draw = FALSE),
		ylab = "data",
		xlab = "normal quantiles"
		)

## ----jordan-sol----------------------------------------------------------
qqmath( ~ points, data = Jordan8687)

## ----joint02-------------------------------------------------------------
f <- function(x) { 6 * x[1] * x[2]^2 }
cubature::adaptIntegrate(f, c(0, 0), c(1, 1))
g <- function(x) { 
    if (x[1] > x[2]) {return(0)}   # set value to 0 if X > Y
    return(f(x))                   # else return joint pdf
    }
cubature::adaptIntegrate(g, c(0, 0), c(1, 1), tol = 0.01)  # get less accuracy

## ------------------------------------------------------------------------
pnorm(-10, 0, 25 / sqrt(3)) 

## ----echo = FALSE--------------------------------------------------------
f <- Vectorize(function(x, y)  mvtnorm::dmvnorm(c(x, y)))
g <- Vectorize(function(x, y)  
  mvtnorm::dmvnorm(
    c(x, y), 
    sigma = rbind(c(1, 0.8), c(0.8, 1))
    ))

## ----echo = FALSE--------------------------------------------------------
plotFun(f(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE, 
        interactive = FALSE,
        par.settings = list (box.3d = list(col = "transparent")),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, z = list(draw = FALSE))
)
plotFun(g(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE, 
        interactive = FALSE,
        par.settings = list (box.3d = list(col = "transparent")),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, z = list(draw = FALSE))
)

## ----echo = FALSE--------------------------------------------------------
plotFun(f(x, y) ~ x + y, x.lim = c(-3.5, 3.5), y.lim = c(-2, 2), npts = 150)
plotFun(g(x, y) ~ x + y, x.lim = c(-3.5, 3.5), y.lim = c(-2, 2), npts = 150)

## ------------------------------------------------------------------------
A1 <- rbind(c(-1, 0), c(-2, -1))
A1 %*% t(A1)

## ------------------------------------------------------------------------
A2 <- rbind(c(3/5, 4/5), c(2, 1))
A2 %*% t(A2)

## ----rho-----------------------------------------------------------------
A <- rbind(c(1, 0), c(-1, 0))
Sigma <- A %*% t(A); Sigma
det(Sigma)
rho <- Sigma[1,2] / (Sigma[1,1] * Sigma[2,2]); rho

## ------------------------------------------------------------------------
max(0, pnorm(0) - pnorm(0))
max(0, pnorm(1) - pnorm(-1))
max(0, pnorm(-1) - pnorm(-2))
max(0, pnorm(1) - pnorm(2))
max(0, pnorm(2) - pnorm(-2))

## ----mvnorm--------------------------------------------------------------
apropos("mvnorm")

## ----mvnorm02------------------------------------------------------------
A <- rbind(c(1, 0, 0), c(1, 1, 0), c(2, 1, 1)); A
Sigma <- A %*% t(A); Sigma
mu <- c(0, 1, 2)
rmvnorm(2, mean = mu, sigma = Sigma)

## ----mvnorm03------------------------------------------------------------
# find q such that Prob( X_1 <= q & X_2 <= q & X_3 <= q) = 0.5
qmvnorm(0.5, mean = mu, sigma = Sigma)

## ----mvnorm04------------------------------------------------------------
# By the result above, this should be just under 0.5
pmvnorm(upper = c(2, 2, 2), mean = mu, sigma = Sigma)
# Prob(all three are between -1 and 1)
pmvnorm(lower = c(-1, -1, -1), upper = c(1, 1, 1), 
        mean = mu, sigma = Sigma)

## ------------------------------------------------------------------------
A <- rbind(c(sqrt(5), 0), c(-3/5 * sqrt(5), 4/5 * sqrt(5)))
A %*% t(A)

## ----mvn-marginal01------------------------------------------------------
A <- rbind(c(1, 0, 0), c(1, 1, 0), c(1, 2, 1))
# covariance matrix
Sigma<- A %*% t(A); Sigma
# marginal covariance matrix
Sigma[-3, -3]

## ------------------------------------------------------------------------
A12 <- A[1:2, 1:2]; A12
Sigma12 <- A12 %*% t(A12); Sigma12

## ----mvn-marginal02, digits = 2, seed = 12345----------------------------
# simulate 3 independent Norm(0,1) vars
Z1 <- rnorm(100000); Z2 <- rnorm(100000); Z3 <- rnorm(100000)
# create the X's from the Z's
X1 <- 1 + Z1; X2 <- 2 + Z1 + Z2; X3 <- 3 + Z1 + 2 * Z2 + Z3
data.frame( `E(X1)` = mean(X1), `E(X2)` = mean(X2),
            `Var(X1)` = var(X1), `Var(X2)` = var(X2),
            `Cov(X1, X2)` = cov(X1, X2), check.names = FALSE)  


## ----mvn-marginal03------------------------------------------------------
Sigma[-2, -2]

## ----mvn-marginal04------------------------------------------------------
data.frame(
  `mean(X3)` = mean(X3), `var(X3)` = var(X3),
  `cov(X1, X3)` = cov(X1, X3), check.names = FALSE)

## ----mvn-marginal05------------------------------------------------------
B <- A[c(1, 3, 2), c(1, 3, 2)]; B
# This is NOT the covariance matrix
B[-3, -3] %*% t(B[-3, -3])
# Nor is this (since it is the same as the matrix above)
A[-2, -2] %*% t(A[-2, -2])

## ----mvn-marginal06------------------------------------------------------
C <- rbind(c(1, 0), c(1, sqrt(5))); C
C %*% t(C)

## ----mvn-marginal07------------------------------------------------------
B <- rbind(c(1, 0, 0), 
           c(1, sqrt(5), 0), 
           c(1, 2/sqrt(5), 1/sqrt(5)))
B %*% t(B)

## ----mvn-conditional01---------------------------------------------------
B <- rbind(c(1, 0), c(1, 1)); B
C <- rbind(c(1,2)); C
Binv <- solve(B); Binv
C %*% Binv

## ----mvn-condidtional02--------------------------------------------------
X3cond <- X3[round(X1) == 3 & round(X2) == 4]
favstats(X3cond)

## ----mvn-conditional03---------------------------------------------------
D <- rbind(c(1, 0), c(2, 1))
D %*% t(D)

## ----mvn-condition04-----------------------------------------------------
X2cond <- X2[round(X1, 1) == 3]
X3cond <- X3[round(X1, 1) == 3]
data.frame(
  `mean(X2|X1 = 3)` = mean(X2cond), `mean(X3|X1 = 3)` = mean(X3cond),
  `var(X2|X1 = 3)` = var(X2cond), `var(X3|X1 = 3)` = var(X3cond),
  `cov(X2, X3 | X1 = 3)` = cov(X2cond, X3cond), check.names = FALSE
  )

## ------------------------------------------------------------------------
Sigma
mu <- c(1, 2, 3); mu
# means
mu[2:3] + Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% (3 - mu[1])
# variance-covariance
Sigma[2:3, 2:3] - Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% Sigma[1, 2:3]


## ----CLT, child="CLT.Rnw", eval=includeChapter[4]------------------------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/CLT-")

## ----mom-unif01----------------------------------------------------------
x <- c(1.6, 2.8, 6.2, 8.2, 8.5, 8.7);  mean(x)

## ----mom-unif02----------------------------------------------------------
x <- c(0.2, 0.9, 1.9, 2.2, 4.7, 5.1); mean(x)

## ----mom-unif-sim01, cache = TRUE----------------------------------------
simulate <- function(size) {
    rdata <- runif(size)
    2 * mean(rdata) < max(rdata)
    }
mean(replicate(1000, simulate(6)))
mean(replicate(1000, simulate(12)))
mean(replicate(1000, simulate(24)))

## ----mom-exp01, echo = FALSE, results="hide"-----------------------------
time <- c(49.0, 60.4, 8.9, 43.4, 34.8, 8.2, 13.6, 11.5, 99.4, 31.9)  
mean(time)
lambda.hat = 1 / mean(time); lambda.hat

histogram( ~ time, n = 10, xlim = c(0,NA),
           density = TRUE, dmath = dexp,
           args = list(rate = lambda.hat)
)

breaks = seq(0, 12, by = 2)^2
histogram( ~ time, n = 10, xlim = c(0,NA), breaks = breaks,
           density = TRUE, dmath = dexp,
           args = list(rate = lambda.hat)
)

## ----mom-exp02-----------------------------------------------------------
time <- c(49.0, 60.4, 8.9, 43.4, 34.8, 8.2, 13.6, 11.5, 99.4, 31.9)  
mean(time)
lambda.hat = 1 / mean(time); lambda.hat

## ----moment--------------------------------------------------------------
moment <- function(k = 1,           # which moment?
            x,                      # data
            centered = TRUE,        # centered on mean?
            na.rm = TRUE)           # remove missing vals?
            {     

    if (na.rm) { x <- x[!is.na(x)] }

    if (length(k) > 1) {  # vectorize this (fancy)
        return(sapply(k, moment, x = x, centered = centered))
    }

    if (centered) { m = mean(x) } else { m = 0 }

    return(sum((x - m)^k) / length(x))
}
x <- (1:10)^2; n <- length(x)
moment(1:2, x, centered = FALSE)
moment(1:2, x, centered = TRUE)

c(mean(x), (n-1) / n * var(x))

## ----mom-norm------------------------------------------------------------
x <- 
  c(57.9, 70.8, 86.3, 92.3, 94.2, 117.0, 118.4, 122.4, 125.8, 134.4)
mean(x)
sd(x)
sqrt(9/10 * var(x))

## ----mom-beta01, seed = 1234---------------------------------------------
beta.mom <- function(x, lower = 0.01, upper = 100) {
    x.bar <- mean (x)
    n <- length(x)
    v <- var(x) * (n-1) / n
    R <- 1 / x.bar - 1

    f <- function(a){ R * x.bar^2 / (a / x.bar + 1) - v }
    u <- uniroot(f, c(lower, upper))

    return(c(shape1 = u$root, shape2 = u$root * R))
}
x <- rbeta(50, 2, 5); beta.mom(x)

## ----mom-beta01a---------------------------------------------------------
# algebraic solutions 
x.bar <- mean(x); x.bar
v <- var(x) * (length(x) - 1) / length(x); v
x.bar * (x.bar * (1 - x.bar) / v - 1)         # alpha = shape1
(1 - x.bar) * (x.bar * (1 - x.bar) / v - 1)   # beta = shape2

## ----sample-means, fig.show = "hide", cache = TRUE-----------------------
# 1000 sample means of samples of size 16 from N(100,12):
sampleMeans <- replicate(5000, mean(rnorm(16, 100, 12)))
mean(sampleMeans)
sd(sampleMeans)
histogram( ~ sampleMeans, n = 20, v = 100,
           density = TRUE, args = list(mean = 100, sd = 3))

## ----sample-means-fig, echo = FALSE, results = "hide", cache = TRUE------
# 1000 sample means of samples of size 16 from N(100,12):
sampleMeans <- replicate(5000, mean(rnorm(16, 100, 12)))
mean(sampleMeans)
sd(sampleMeans)
histogram( ~ sampleMeans, n = 20, v = 100,
           density = TRUE, args = list(mean = 100, sd = 3))

## ----eval = FALSE--------------------------------------------------------
## mean(rnorm(16, 100, 12))

## ----mom-beta-sim01, fig.show = "hide", cache = TRUE---------------------
Results <- do(1000) * beta.mom(rbeta(50, 2, 5))
histogram( ~ shape1, data = Results, type = "density", v = 2)
histogram( ~ shape2, data = Results, type = "density", v = 5)

## ----mom-beta-sim01-fig, echo = FALSE, cache = TRUE----------------------


## ----mom-beta-sim02, eval = FALSE----------------------------------------
## xyplot(shape2 ~ shape1, data = Results, alpha = 0.4,
## 	   panel = function(x, y, ...){
##             panel.abline(a = 0, b = 5/2)
##             panel.xyplot(x, y, ...)
##             })
## histogram( ~ shape2 / shape1, data = Results, type = "density", v = 2.5)

## ----mom-beta-sim-02-fig, echo = FALSE-----------------------------------
xyplot(shape2 ~ shape1, data = Results, alpha = 0.4, 
	   panel = function(x, y, ...){
            panel.abline(a = 0, b = 5/2)
            panel.xyplot(x, y, ...)
            })
histogram( ~ shape2 / shape1, data = Results, type = "density", v = 2.5)

## ----miaa-ft-betas-------------------------------------------------------
# This gives the method of moments estimates 
# for the full data set
beta.mom(MIAA05$FTPct)

## ----miaa-ft-beta--------------------------------------------------------
miaa <- MIAA05
length(miaa$FTPct)
beta.mom(miaa$FTPct)
# remove players who took no shots
someshots <- miaa$FTPct[miaa$FTA >= 1]
length(someshots)
beta.mom(someshots) -> bmom1; bmom1
qqmath(someshots, 
    dist = function(x)qbeta(x, bmom1["shape1"], bmom1["shape2"]))
# remove players with fewer than 10 shots
tenshots <- miaa$FTPct[miaa$FTA >= 10]
length(tenshots)
beta.mom(tenshots) -> bmom2; bmom2
qqmath(tenshots, 
    dist = function(x)qbeta(x, bmom2["shape1"], bmom2["shape2"]))

## ----miaa-fg-beta-sol----------------------------------------------------
miaa <- MIAA05
length(miaa$FGPct)
beta.mom(miaa$FGPct)
# remove players who took no shots 
someshots <- miaa$FGPct[miaa$FGA >= 1]
length(someshots)
beta.mom(someshots) -> bmom1; bmom1
qqmath(someshots, 
    dist = function(x)qbeta(x, bmom1["shape1"], bmom1["shape2"]))
# remove players with fewer than 10 shots
tenshots <- miaa$FTPct[miaa$FGA >= 10]
length(tenshots)
beta.mom(tenshots) -> bmom2; bmom2
qqmath(tenshots, 
    dist = function(x)qbeta(x, bmom2["shape1"], bmom2["shape2"]))

## ----mom-life01----------------------------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
l <- 1/ mean(life01); l

## ----mom-life02----------------------------------------------------------
m <- mean(life01); m
n <- length(life01); n
v <- var(life01) * (n-1) / n; v
# alpha
m^2 / v
# lambda
m / v

## ----sample-srs, seed = 1234---------------------------------------------
sample(1:30, 15)                   # 15 random numbers in 1-30
sample(1:30, 15, replace = TRUE)   # iid random sample

## ----sample-do-----------------------------------------------------------
sample(vcd::VonBort, 10)           # SRS of size 10
# mean of SRS
mean( ~ deaths, data = sample(vcd::VonBort, 10))  
# mean of an iid random sample
mean( ~ deaths, data = resample(vcd::VonBort, 10))  
# means of 3 SRSs using do()
do (3) * mean(~ deaths, data = sample(vcd::VonBort, 10))
# means of 3 SRSs using replicate()
replicate(3, mean(~ deaths, data = sample(vcd::VonBort, 10)))
mean( ~ deaths, data = vcd::VonBort)    # mean of entire data set
histogram( ~ mean, 
           data = do (1000) * mean(~ deaths, data = sample(vcd::VonBort, 10)))

## ----law-large-numbers, eval = FALSE-------------------------------------
## expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
##   mutate(x = rexp(6 * 1000)) %>%
##   group_by(run) %>% arrange(rep) %>%
##   mutate(runningMean = cumsum(x) / 1:length(x)) %>%
## xyplot(runningMean ~ rep | run, data = .,
##        ylab = "running mean", xlab = "", type = "l",
##        ylim = c(0, 3),
##        panel = function(...){
##          panel.abline(h = 1, col = "gray70")
##          panel.xyplot(...)
##        })

## ----law-large-numbers-fig, echo = FALSE, eval = TRUE, cache = TRUE, opts.label = "fig1"----
expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
  mutate(x = rexp(6 * 1000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
xyplot(runningMean ~ rep | run, data = ., 
       ylab = "running mean", xlab = "", type = "l",
       ylim = c(0, 3), 
       panel = function(...){ 
         panel.abline(h = 1, col = "gray70") 
         panel.xyplot(...) 
       })

## ----lln-cauchy, eval = FALSE, cache = TRUE, seed = 123------------------
## expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
##   mutate(x = rcauchy(6 * 10000)) %>%
##   group_by(run) %>% arrange(rep) %>%
##   mutate(runningMean = cumsum(x) / 1:length(x)) %>%
## xyplot(runningMean ~ rep | run, data = ., ylim = c(-10, 10),
##        ylab = "running mean", xlab = "", type = "l",
##        panel = function(...){
##          panel.abline(h = 0, col = "gray70")
##          panel.xyplot(...)
##        })

## ----lln-cauchy-fig, cache = TRUE, echo = FALSE, opts.label = "fig1"-----
expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
  mutate(x = rcauchy(6 * 10000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
xyplot(runningMean ~ rep | run, data = ., ylim = c(-10, 10),
       ylab = "running mean", xlab = "", type = "l",
       panel = function(...){ 
         panel.abline(h = 0, col = "gray70") 
         panel.xyplot(...) 
       })

## ----lln-cauchy-seed, fig.keep="none"------------------------------------
set.seed(123)
x <- rcauchy(10000)
runningMean <- cumsum(x) / 1:length(x)
xyplot(runningMean ~ 1:10000,
       ylab = "running mean", xlab = "n", type = "l")

## ----clt-prob02----------------------------------------------------------
pnorm(3, sd = 2) - pnorm(-3, sd = 2)

## ----clt-prob-finite-samples---------------------------------------------
x <- c(1, 6, 6, 8, 9)
mu <- sum(x * 0.2); mu                 # population mean
v <- sum(x^2 * 0.2) - mu^2; v          # population variance
pairsums <- outer(x, x, "+")           # compute 25 sums
pairmeans <- pairsums / 2

# sampling distribution with SRS
srs.means <- as.vector(pairmeans[lower.tri(pairmeans)]); srs.means
iid.means <- as.vector(pairmeans); iid.means

srs.mean <- sum(srs.means * 0.1); srs.mean
srs.var <- sum(srs.means^2 * 0.1) - srs.mean^2; srs.var
v / 2 * (5-2) / (5-1)
sqrt(v / 2 * (5-2) / (5-1))

var(srs.means)   # N.B: This is the INCORRECT variance

## ----clt-prob-finite-sample----------------------------------------------
x <- c(1, 2, 4, 4, 9)
mu <- sum(x * 0.2); mu                 # population mean
v <- sum(x^2 * 0.2) - mu^2; v          # population variance
pairsums <- outer(x, x, "+")           # compute 25 sums
pairmeans <- pairsums / 2

# sampling distribution with SRS
srs.means <- as.vector(pairmeans[lower.tri(pairmeans)]); srs.means
iid.means <- as.vector(pairmeans); iid.means

srs.mean <- sum(srs.means * 0.1); srs.mean
srs.var <- sum(srs.means^2 * 0.1) - srs.mean^2; srs.var
v / 2 * (5-2) / (5-1)
sqrt(v / 2 * (5-2) / (5-1))

var(srs.means)   # INCORRECT variance

# sampling distribution with iid sample
iid.mean <- sum(iid.means * 0.04); iid.mean
iid.var <- sum(iid.means^2 * 0.04) - iid.mean^2; iid.var
v / 2
sqrt(v / 2)

var(iid.means)   # INCORRECT variance

## ----unif12, eval = FALSE, cache = TRUE----------------------------------
## sampleSums <- replicate(2000, sum(runif(12, -0.5, 0.5)))
## qqmath( ~ sampleSums)
## histogram( ~ sampleSums)

## ----unif12-fig, echo = FALSE, cache = TRUE------------------------------
sampleSums <- replicate(2000, sum(runif(12, -0.5, 0.5)))
qqmath( ~ sampleSums)
histogram( ~ sampleSums)

## ----betaCLT, eval = FALSE, cache = TRUE---------------------------------
## BetaSims <-
##   expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
##   group_by(rep, size) %>% mutate(mean = mean(rbeta(size, 0.5, 0.5)))
## qqmath(    ~ mean | factor(size), data = BetaSims,
##            scales = list(relation = "free"))
## histogram( ~ mean | factor(size), data = BetaSims, n = 25, density = TRUE)

## ----betaPDF-fig, echo = FALSE, cache = TRUE-----------------------------
plotDist("beta", shape1 = 0.5, shape2 = 0.5, ylim = c(0, 5))

## ----betaCLT-fig, echo = FALSE-------------------------------------------
BetaSims <-
  expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
  group_by(rep, size) %>% mutate(mean = mean(rbeta(size, 0.5, 0.5)))
qqmath(    ~ mean | factor(size), data = BetaSims, 
           scales = list(relation = "free"))
histogram( ~ mean | factor(size), data = BetaSims, n = 25, density = TRUE)

## ----binomial-normal-hist, echo = FALSE, opts.label = "figbig"-----------
p <- ppoints(50);
n <- rep(c(10, 40, 80, 800), each = length(p))
p <- rep(p, times = 4)

pi <- rep(c(0.5, 0.3, 0.1, 0.05), each = length(p))
p <- rep(p, times = 4)
n <- rep(n, times = 4)

p <- xyplot(
        qbinom(p, n, pi) ~ qnorm(p, n * pi, sqrt(n * pi * (1-pi))) |
        paste("n=", n, sep = "") * paste("pi=", pi, sep = ""),
        scales = list(relation = "free"),
        cex = 0.6,
    ylab = expression(qbinom(p, n, pi)),
    xlab = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi)))),
    panel = function(x, y, ...){
        panel.abline(0, 1, ...);
        panel.xyplot(x, y, ...);
    })
latticeExtra::useOuterStrips(p)

## ----binomialCLT, echo = FALSE-------------------------------------------
xyplot(dbinom(0:20, 20, 0.1) ~ 0:20, type = "h", xlim = c(0,10),
            lwd = 2,
            xlab = expression(x),
            main = "Binomial vs Normal(n=20, pi=0.10)",
            ylab = "density",
            panel = function(x, y, ...){
                panel.xyplot(x, y, col = "black", ...);
                panel.mathdensity(dnorm,
                                  list(mean = 2, sd = sqrt(0.1 * 0.9 * 20)), ...);
            })

## ----continuity-correction-----------------------------------------------
# P(55 <= X <= 65)
pbinom(65, 100, 0.6) - pbinom(54, 100, 0.6)        
# without continuity correction:
diff(pnorm(c(55, 65), 60, sqrt(100 * 0.6 * 0.4)))
# with continuity correction:
diff(pnorm(c(54.5, 65.5), 60, sqrt(100 * 0.6 * 0.4)))

## ----binomial-ztest------------------------------------------------------
# "exact" p-value
binom.test(60, 100);                             
# approximate p-value
z <- (0.6 - 0.5) / sqrt(0.5 * 0.5 / 100); z
2 * (1 - pnorm(z))                         

# approximate p-value with continuity correction
z <- (0.595 - 0.5) / sqrt(0.5 * 0.5 / 100); z   # 0.595 = 59.5 / 100
2 * (1 - pnorm(z))                         

# R can automate the approximate version too:
prop.test(60, 100)            # uses continuity correction by default
prop.test(60, 100, correct = FALSE)     # turn off continuity correction

## ------------------------------------------------------------------------
binom.test(465, 980)
 prop.test(465, 980)

## ------------------------------------------------------------------------
1 - pnorm(160/3, mean = 50, sd = 5 / sqrt(3))

## ----eval = FALSE--------------------------------------------------------
## z.test <- function (x, alternative = c("two.sided", "less", "greater"),
##     				mu = 0, sigma = 1, conf.level = 0.95)
## {
##     DNAME <- deparse(substitute(x))      # record name of data coming in
##     alternative <- match.arg(alternative)      # fancy argument matching
## 
##     # your code goes here			
## }

## ----eval = FALSE, tidy = FALSE------------------------------------------
##     Z <- "???" ; names(Z) <- "z"
##     SIGMA <- sigma; names(SIGMA) <- "sigma"
##     MU <- mu; names(MU) <- "mean"
##     ESTIMATE <- "???" ; names(ESTIMATE) <- "sample mean"
##     CINT <- "???"; attr(CINT, "conf.level") <- conf.level
##     PVAL <- "???";
## 		
##     structure(list(statistic = Z, parameter = SIGMA, p.value = PVAL,
##         conf.int = CINT, estimate = ESTIMATE, null.value = MU,
##         alternative = alternative, method = "Z test for a mean",
##         data.name = DNAME),
##         class = "htest")

## ----z-test--------------------------------------------------------------
z.test <- function (x, 
			alternative = c("two.sided", "less", "greater"), 
    		mu = 0, sigma = 1, conf.level = 0.95) 
{
    DNAME <- deparse(substitute(x))      # record name of data coming in
    alternative <- match.arg(alternative)      # fancy argument matching

	n <- length(x)
	x.bar <- mean(x, na.rm = TRUE)
	se <- sigma / sqrt(n)

	alpha <- 1 - conf.level
	p <- switch(alternative, 
		two.sided = c(alpha/ 2, 1 - alpha / 2),
		less = c(0, 1 - alpha),
		greater = c(alpha, 1)
		)
	z.star <- qnorm(p)

    Z <- (x.bar - mu) / se; names(Z) <- "z"
    SIGMA <- sigma; names(SIGMA) <- "sigma"
    MU <- mu; names(MU) <- "mean"
    ESTIMATE <- x.bar; names(ESTIMATE) <- "sample mean"
    CINT <- x.bar + z.star * se; 
		attr(CINT, "conf.level") <- conf.level            
    PVAL <- switch(alternative,
		two.sided = 2 * pnorm( - abs(Z)),
		less = pnorm(Z),
		greater = 1 - pnorm(Z) 
		)
		
    structure(list(statistic = Z, parameter = SIGMA, p.value = PVAL, 
        conf.int = CINT, estimate = ESTIMATE, null.value = MU, 
        alternative = alternative, method = "Z test for a mean", 
        data.name = DNAME),        
        class = "htest")
}

## ----free-weights--------------------------------------------------------
z <- (4.96 - 5.0) / (0.05 / sqrt(10)); z    # test statistic
2 * (pnorm(z))                              # 2-sided p-value
2 * (1 - pnorm(abs(z)))                     # 2-sided p-value again

## ----z-ci----------------------------------------------------------------
zci <- function(x, sd = 100, conf.level = 0.95) { 
  alpha = 1 - conf.level
  n = length(x)
  zstar <- - qnorm(alpha / 2)
  interval <- mean(x)  + c(-1,1) * zstar * sd / sqrt(n)
  return(list(conf.int = interval, estimate = mean(x)))
}

## ----ci-vis, eval = TRUE, cache = TRUE, fig.keep = "none", seed = 1234----
# simulate 100 intervals and plot them. 
CIsim(n = 20, samples = 100, estimand = 500, 
      rdist = rnorm, args = list(mean = 500, sd = 100),
      method = zci, method.args = list(sd = 100))

## ----ci-vis-fig, echo = FALSE, message = FALSE, cache = TRUE, seed = 1234----
# simulate 100 intervals and plot them. 
CIsim(n = 20, samples = 100, estimand = 500, 
      rdist = rnorm, args = list(mean = 500, sd = 100),
      method = zci, method.args = list(sd = 100))

## ----simulate-ci, cache = TRUE-------------------------------------------
# an example CI from a sample of size 20
zci(rnorm(20, 500, 100))
# 10,000 simulated samples each of size 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = rnorm, 
      args = list(mean = 500, sd = 100),
	    estimand = 500, method = zci, method.args = list(sd = 100))

## ----simulate-ci-unif, cache = TRUE--------------------------------------
mu = 1/2; v = 1/12           # mean and variance
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = runif, estimand = mu,
	method = zci, method.args = list(sd = sqrt(v)))

## ----simulate-ci-beta, cache = TRUE--------------------------------------
mu <- 0.4 / (0.4 + 0.6); mu           # mean for beta dist
v <- (0.4 * 0.6) / ((0.4 + 0.6)^2 * (0.4 + 0.6 + 1)); v  # var for beta dist
#
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rbeta, args = list(shape1 = 0.4, shape2 = 0.6),
	    estimand = mu, method = zci, method.args = list(sd = sqrt(v)))

## ----simulate-ci-exp, cache = TRUE---------------------------------------
rate = 1/10
v = (1 / rate)^2                                   # var of exponential
mu = 10                                            # mean of exponential
zci(rexp(20, rate), sd = sqrt(v))$conf.int         # an example CI
#
# 10,000 simulated samples of size 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rexp, args = list(rate = rate), estimand = mu, 
      method = zci, method.args = list(sd = sqrt(v)))

## ------------------------------------------------------------------------
prop.test(9458,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9457,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9458,  10000, p = .95) %>% pval()
prop.test(9457,  10000, p = .95) %>% pval()
prop.test(9456,  10000, p = .95) %>% pval()
binom.test(9457, 10000, p = .95) %>% pval()
binom.test(9456, 10000, p = .95) %>% pval()

## ------------------------------------------------------------------------
1.96 * sqrt(0.95 * 0.05 / 10000)

## ----99CI----------------------------------------------------------------
zstar <- - qnorm(0.005); zstar
se <- 2 / sqrt(25); se
zstar * se;
10 + c(-1,1) * zstar * se                      # confidence interval

## ----unif-ci-------------------------------------------------------------
x <- c(1.6, 2.8, 6.2, 8.2, 8.5, 8.7)
c(max(x), max(x) / 0.05^(1/6))

## ----unif-ci-sim---------------------------------------------------------
Sims <-
  expand.grid(n = c(5, 10, 20, 50), rep = 1:10000, theta = 10) %>%
  group_by(n, rep, theta) %>%
  mutate(
    lower = max(runif(n, 0, theta)),
    upper = lower / 0.05^(1/n),
    cover = upper > theta
  )
prop(cover ~ n, data = Sims)

## ------------------------------------------------------------------------
zstar <- qnorm(0.05)
c(-Inf, 8 + zstar * 3 / sqrt(16))
c(8 - zstar * 3 / sqrt(16), Inf)

## ------------------------------------------------------------------------
myci <- function(x, y) {
  setNames(
    mean(x) - mean(y) + c(-1, 1) * sqrt( var(x)/length(x) + var(y)/length(y) ),
    c("lower", "upper")
  )
}
Intervals <- 
  (do(10000) * myci(rnorm(10, 9, 2), rnorm(5, 8, 3))) %>%
  mutate(
    estimate = (upper + lower) / 2,
    me = 0.5 * (upper - lower),
    D = abs(1 - estimate),
    ratio = D / me
  )
R <- qdata( ~ ratio, data = Intervals, p = 0.95)["quantile"]; R
uniroot(function(df) qt(0.975, df = df) - R, c(1,100)) %>% value()

## ----uvec-sol,tidy = FALSE-----------------------------------------------
x <- c(3, 4, 4, 7, 7)
mean(x)
v <- x - mean(x)
u1 <- c(1,1,1,1,1) / sqrt(5)
u2 <- c(1,-1,0,0,0) / sqrt(2)
u3 <- c(1,1,-2,0,0) / sqrt(6)
u4 <- c(1,1,1,-3,0) / sqrt(12)
u5 <- c(1,1,1,1,-4) / sqrt(20)
ulist <- list(u1,u2,u3,u4,u5)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
xList <- lapply(ulist, function(u) project(x,u)); xList
vList <- lapply(ulist, function(u) project(v,u)); vList
all.equal(xList[-1], vList[-1])

## ----t-dist, echo = FALSE------------------------------------------------
x <- seq(-5, 5, by = 0.05)
l <- length(x)
ddd <- data.frame(
    pdf = c(dnorm(x), dt(x, df = 1), dt(x, df = 2), dt(x, df = 4), dt(x, df = 10)),
    distribution = rep(c(1000, 1, 2, 4, 10), each = l),
    x = rep(x, times = 5)
    )

ddd$distribution <- 
  factor(ddd$distribution,
         labels = c("df=1", "df=2", "df=4", "df=10", "normal")
  )

line.list <- list(
  lty = c(1, 1, 1, 1, 1), # lty = c(1, 2, 3, 4, 1), 
  lwd = c(2, 2, 2, 2, 2),
  col = paste("gray", c(80, 60, 40, 20, 5), sep = "")  
)

xyplot(pdf ~ x, ddd,
    groups = distribution,
    type = "l",
    lattice.options = list( 
        pch = 16, 
        superpose.line = line.list
        ),
    # main = "pdfs of standard normal and t-distributions",
    lwd = line.list$lwd,
    lty = line.list$lty,
    col = line.list$col,
    key = list(
        lines = line.list,
        text = list(
            lab = levels(ddd$distribution)
            ),
        columns = 3
        )
    )

## ----t-test-01-----------------------------------------------------------
t <- (10.3 - 10)/ (0.4 / sqrt(10)); t       # test statistic
2 * pt(-abs(t), df = 9);         # p-value using t-distribution
2 * pnorm(-abs(t));              # "p-value" using normal distribution

## ----tCI-01--------------------------------------------------------------
tstar <- qt(0.975, df = 9); tstar
10.3 + c(-1,1) * tstar * 0.4 / sqrt(10)

## ----t-test-iris---------------------------------------------------------
# for CI; p-value not interesting here
t.test( ~ Sepal.Width, data = iris %>% filter(Species == "virginica"))       
# this gives a more interesting p-value
t.test( ~ Sepal.Width, data = iris %>% filter(Species == "virginica"), mu = 3)   

## ----uvec-sol01, tidy = FALSE--------------------------------------------
x <- c(3, 4, 5, 8)
mean(x)
var(x)
u1 <- .5 * c(1,1,1,1)
u2 <-  1 / sqrt(2) * c(1,-1,0,0)
u3 <-  1 / sqrt(6) * c(1,1,-2,0)
u4 <-  1 / sqrt(12) * c(1,1,1,-3)
ulist <- list(u1, u2, u3, u4)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
c(dot(u1, u2), dot(u1, u3), dot(u1, u4), dot(u2, u3), dot(u2, u4), dot(u3, u4))
pList <- lapply(ulist, function(u) project(x, u)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x,x))[2:4])
3 * var(x)

## ----uvec-sol02,tidy = FALSE---------------------------------------------
x <- c(3, 4, 5, 8)
mean(x)
v <- x - mean(x)
w1 <- .5 * c(1,  1,  1,  1)
w2 <- .5 * c(1,  1, -1, -1)
w3 <- .5 * c(1, -1, -1,  1)
w4 <- .5 * c(1, -1,  1, -1)
wlist <- list(w1, w2, w3, w4)
vlength <- function(x) sqrt(dot(x, x))
sapply(wlist, vlength)
c(dot(w1, w2), dot(w1, w3), dot(w1, w4), dot(w2, w3), dot(w2, w4), dot(w3, w4))
pList <- lapply(wlist, function(w)  project(x, w)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x,x))[2:4])
3 * var(x)

## ----uvec-sol03,tidy = FALSE---------------------------------------------
x <- c(3, 4, 5, 8)
mean(x)
v <- x - mean(x)
w1 <- c(1, 1, 1, 1) / 2
w2 <- c(1, 1,-1,-1) / 2
w3 <- c(1,-1, 0, 0) / sqrt(2)
w4 <- c(0, 0, 1,-1) / sqrt(2)
wlist <- list(w1, w2, w3, w4)
sapply(wlist, vlength)
c(dot(w1, w2), dot(w1, w3), dot(w1, w4), dot(w2, w3), dot(w2, w4), dot(w3, w4))
pList <- lapply(wlist, function(w)  project(x,w)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x, x))[2:4])
3 * var(x)

## ----sepal-width-cint----------------------------------------------------
iris %>% group_by(Species) %>%
  do(data.frame(as.list( 
	 confint(t.test( ~ Sepal.Width, data= .))
  )))

## ----sepal-length-cint---------------------------------------------------
iris %>% group_by(Species) %>%
  do(data.frame(as.list( 
	 confint(t.test( ~ Sepal.Length, data= .))
  )))

## ----sepal-ratio-cint----------------------------------------------------
bind_rows(
  lapply(
    levels(iris$Species),
    function(s) { 
      confint(t.test( ~ Sepal.Length / Sepal.Width, 
                      data = filter(iris, Species == s))) %>%
        mutate(species = s)
    }
  )
)

## ----prob-iris-t-test----------------------------------------------------
with(iris, levels(Species))
with(iris, t.test(Sepal.Length[Species == levels(Species)[1]]))
with(iris, t.test(Sepal.Length[Species == levels(Species)[2]]))
with(iris, t.test(Sepal.Length[Species == levels(Species)[3]]))

## ----prob-shoe-size-CI---------------------------------------------------
qt(0.975, df = 255) * 2 / sqrt(256)

## ----uniroot1------------------------------------------------------------
f <- function(n) { qt(0.975, n-1) * 2 / sqrt(n) - 0.25}
uniroot(f, c(10, 1000))

## ----t-simulations, cache = TRUE-----------------------------------------
# an example CI from a sample of size 20
confint(t.test(rnorm(20, 500, 100)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 500, 
	rdist = rnorm, args = list(mean = 500, sd = 100))

## ----heavy-tails, echo = FALSE-------------------------------------------
x <- seq(-7, 10, by = 0.10)
l <- length(x)
ddd <- data.frame(
    pdf = c(dnorm(x, 0, sqrt(3)), dt(x, df = 3)),
    cdf = c(pnorm(x, 0, sqrt(3)), pt(x, df = 3)),
    distribution = rep(c(1000, 3), each = l),
    x = rep(x, times = 2)
    )

ddd$distribution <- factor(ddd$distribution,
    labels = c("T(3)", "Norm")
    )

line.list <- list(
            lty = c(1, 1),
            lwd = c(1.5, 1.5),
            col = trellis.par.get("superpose.line")$col[1:2]
            )

xyplot(pdf ~ x, ddd,
    groups = distribution,
    main = "PDFs",
    type = "l",
    xlim = c(0, 7),
    ylim = c(-0.005, 0.40),
    lattice.options = list( 
        superpose.line = line.list
        ),
    lwd = line.list$lwd,
    lty = line.list$lty,
    col = line.list$col,
    key = list(
        lines = line.list,
        text = list(
            lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
            ),
        columns = 2
        )
    )

xyplot(cdf ~ x, ddd,
    groups = distribution,
    main = "CDFs",
    type = "l",
    xlim = c(-3, 7),
    ylim = c(0, 1),
    lattice.options = list( 
        superpose.line = line.list
        ),
    lwd = line.list$lwd,
    lty = line.list$lty,
    col = line.list$col,
    key = list(
        lines = line.list,
        text = list(
            lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
            ),
        columns = 2
        )
    )

xyplot(pdf ~ x, ddd,
    groups = distribution,
    main = "PDFs",
    type = "l",
    xlim = c(3, 9),
    ylim = c(-0.005, 0.06),
    lattice.options = list( 
        superpose.line = line.list
        ),
    lwd = line.list$lwd,
    lty = line.list$lty,
    col = line.list$col,
    key = list(
        lines = line.list,
        text = list(
            lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
            ),
        columns = 2
        )
    )

xyplot(cdf ~ x, ddd,
    groups = distribution,
    main = "CDFs",
    type = "l",
    xlim = c(3, 9),
    ylim = c(0.98, 1),
    lattice.options = list( 
        superpose.line = line.list
        ),
    lwd = line.list$lwd,
    lty = line.list$lty,
    col = line.list$col,
    key = list(
        lines = line.list,
        text = list(
            lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
            ),
        columns = 2
        )
    )

## ----t-robust-heavytails, cache = TRUE-----------------------------------
# an example CI from a sample of size 20
confint(t.test(rt(20, 3)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 0, 
      rdist = rt, args = list(df = 3))

## ----t-robust-exp, cache = TRUE------------------------------------------
# an example CI from a sample of size 20
confint(t.test(rexp(20, 1/10)))
# 10,000 simulated samples of sizes 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 10, 
      rdist = rexp, args = list(rate = 1/10))

## ----t-robust-qq, eval = FALSE, cache = TRUE-----------------------------
## ExpSims <-
##   expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rexp(n), mu = 1)),
##     dist = paste0("Exp(1); n=", n))
## 
## TSims <-
##   expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rt(n, df = 3), mu = 0)),
##     dist = paste0("t(3); n=", n))
## 
## xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims),
##          dist = qunif, idline = TRUE, cex = 0.4, type = "l")
## xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims),
##          dist = qunif, idline = TRUE, cex = 0.4, type = "l",
##          xlim = c(0, 0.2), ylim = c(0, 0.2))

## ----t-robust-qq-fig, echo = FALSE, opts.label = "fig1", cache = TRUE----
ExpSims <-
  expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rexp(n), mu = 1)), 
    dist = paste0("Exp(1); n=", n))

TSims <-
  expand.grid(n = c(10, 20, 40, 100), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rt(n, df = 3), mu = 0)), 
    dist = paste0("t(3); n=", n))

xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims), 
         dist = qunif, idline = TRUE, cex = 0.4, type = "l")
xqqmath( ~ pval | dist, data = bind_rows(ExpSims, TSims),
         dist = qunif, idline = TRUE, cex = 0.4, type = "l",
         xlim = c(0, 0.2), ylim = c(0, 0.2))

## ----miaa-sol------------------------------------------------------------
set.seed(12345)
Intervals <- 
  do(20) * 
  confint(t.test( ~ PTSG, data = sample(MIAA05, 15), conf.level = .90))
mu <- mean( ~ PTSG, data = MIAA05)
tally( ~ (lower <= mu & mu <= upper), data = Intervals)

## ------------------------------------------------------------------------
WaldSims <- 
  CIsim(2000, n = c(5, 10, 20, 40), 
        method = binom.test, method.args = list(ci.method = "Wald"),
        rdist = rbinom, args = list(size = 1, prob = 0.2),
        estimand = 0.2)

## ----wald-score-compare, echo = FALSE------------------------------------
# function to compute LR statistic
LR = function(pi.0, y, N, alpha) { 
  pi.hat = y / N; L0 = 0; L1 = 0 
  if (pi.0 < 1) L0 = L0 + (N - y) * log(1 - pi.0)
  if (pi.0 > 0) L0 = L0 + y * log(pi.0)
  if (pi.hat > 0) L1 = L1 + y * log(pi.hat)
  if (pi.hat < 1) L1 = L1 + (N - y) * log(1 - pi.hat)
  LR = 2 * (L1 - L0)
  return(LR)
}

# function used in uniroot to find lower and upper bounds 
# of confidence interval
LRCI = function(pi.0, y, N, alpha) {  
  pi.hat = y / N; L0 = 0; L1 = 0 
  if (pi.0 < 1) L0 = L0 + (N-y) * log(1-pi.0)
  if (pi.0 > 0) L0 = L0 + y * log(pi.0)
  if (pi.hat > 0) L1 = L1 + y * log(pi.hat)
  if (pi.hat < 1) L1 = L1 + (N-y) * log(1-pi.hat)
  LR = 2 * (L1-L0)
  LR-qchisq(1-alpha, df = 1)
}

# function used in uniroot to compute lower bound CI of mid-P 
# version of clopper-pearson exact test
midpL = function(pi.0, y, N, alpha) {  
  lowerbound = sum(dbinom(y:N, N, pi.0)) - 0.5 * dbinom(y, N, pi.0)- alpha / 2
  return(lowerbound)
}

# function used in uniroot to compute upper bound CI of mid-P 
# version of clopper-pearson exact test
midpU = function(pi.0, y, N, alpha) { 
  upperbound = sum(dbinom(0:y, N, pi.0)) - 0.5 * dbinom(y, N, pi.0)- alpha / 2
  return(upperbound)
}

n = 35
y = 0:n
pi.hat = y / n
pi.wig = (y + 2) / (n + 4)

tmp = matrix(0, length(seq(0.001, 0.999, by = 0.001)), 8)
# (n + 1) by 2 matrices to hold confindence bounds 
#    (first col--lower bound, second col -- upper bound)
#  W -wald, S - score, E - exact, L - LR, BJ = bayes, jeffrey's prior, 
#  BU - bayes uniform prior, MP - mid p, Wil = Wilson 
W = matrix(0, n + 1, 2)   
S = W    
E = W
L = W
BJ = W
BU = W
MP = W
Wil = W

tmps = lapply(y, stats::prop.test, n, correct = FALSE) #compute confidence bounds for score 
tmpe = lapply(y, stats::binom.test, n)          #compute confidence bounds for exact

# compute lower/upper confidence bounds for wald
W[, 1] = pi.hat  - qnorm(0.975) * sqrt(pi.hat * (1-pi.hat) / n)  
W[, 2] = pi.hat  + qnorm(0.975) * sqrt(pi.hat * (1-pi.hat) / n)

# compute lower/upper confidence bounds for Wilson
Wil[, 1] = pi.wig  - qnorm(0.975) * sqrt(pi.wig * (1-pi.wig) / (n + 4))  
Wil[, 2] = pi.wig  + qnorm(0.975) * sqrt(pi.wig * (1-pi.wig) / (n + 4))

for (i in 1:(n + 1)) {
   S[i, ] = tmps[[i]]$conf.int   #extract confidence interval for score
   E[i, ] = tmpe[[i]]$conf.int   # extract conf. int. for exact
   if (y[i] == 0){
     L[i, 1] = 0
     MP[i, 1] = 0
   }
   else { 
     L[i, 1] =  uniroot(f = LR, interval = c(0.000001, y[i] / n), N = n, y = y[i],
                       alpha = 0.05)$root            # lower bound for LR
       MP[i, 1] = uniroot(f = midpL, interval = c(0.000001, 0.999999),
                       N = n, y = y[i], alpha = 0.05)$root # lower bound for mid-P
  }
  if (y[i] == n) {
     L[i, 2] = 1
     MP[i, 2] = 1
  }
  else { 
     L[i, 2] = uniroot(f = LR, interval = c(y[i] / n, 0.999999), N = n, y = y[i],
                      alpha = 0.05)$root             # upper bound for LR
     MP[i, 2] = uniroot(f = midpU, interval = c(0.000001, 0.999999),
                       N = n, y = y[i], alpha = 0.05)$root #upper bound for mid-P
  }
}

BJ[, 1] = qbeta(0.025, 0.5 + y, n + 0.5-y)  # lower bounds, bayes, jeffrey's prior
BJ[, 2] = qbeta(0.975, 0.5 + y, n + 0.5-y)  # upper bounds
BU[, 1] = qbeta(0.025, 1 + y, n + 1-y)    # lower bounds bayes, uniform prior
BU[, 2] = qbeta(0.975, 1 + y, n + 1-y)    # upper bounds

cnt = 1
# probabilities from the binomial y = (0, 1, 2, ..., 25)
#probs = dbinom(y, n, pi.0) 

pi.values <- seq(0.001, 0.999, by = 0.001)
for (pi.0 in pi.values) {
# calculate coverage rates, put into matrix tmp
 probs = dbinom(y, n, pi.0)
 tmp[cnt, 1] = (S[, 1] < pi.0 & pi.0 < S[, 2]) %*% probs
 tmp[cnt, 2] = (W[, 1] < pi.0 & pi.0 < W[, 2]) %*% probs
 tmp[cnt, 3] = (E[, 1] < pi.0 & pi.0 < E[, 2]) %*% probs
 tmp[cnt, 4] = (L[, 1] < pi.0 & pi.0 < L[, 2]) %*% probs
 tmp[cnt, 5] = (BJ[, 1] < pi.0 & pi.0 < BJ[, 2]) %*% probs
 tmp[cnt, 6] = (BU[, 1] < pi.0 & pi.0 < BU[, 2]) %*% probs
 tmp[cnt, 7] = (MP[, 1] < pi.0 & pi.0 < MP[, 2]) %*% probs
 tmp[cnt, 8] = (Wil[, 1] < pi.0 & pi.0 < Wil[, 2]) %*% probs
 cnt = cnt + 1
}

nn <- length(pi.values)
coverage <- data.frame(
	pi = rep(pi.values, times = 3),
	coverage = c(tmp[, 1], tmp[, 2], tmp[, 3]),
	method = factor(rep(c("Score", "Wald", "Clopper-Pearson"), each = nn),
				levels = c("Wald", "Clopper-Pearson", "Score"))
	)

# below, opens a pdf file creates various plots shown in lecture  
# and closes the PDF device 

trellis.par.set(theme = col.fastR(bw = TRUE));
if(FALSE) {
matplot(seq(0.001, 0.999, by = 0.001), tmp[, 1:3], type = "l",
    lty = 1,
    col = trellis.par.get("superpose.line")$col[1:3],
    main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
    xlab = expression(pi),
    ylab = "Coverage Rate",
    lwd = 2,
    ylim = c(0.8, 1));
    abline(h = 0.95);
    legend(0.35, 0.875, c("Score", "Wald", "Clopper-Pearson")[c(3, 1, 2)], 
        col = trellis.par.get("superpose.line")$col[c(3, 1, 2)],
        lwd = 2,
        lty = 1,
        cex = 1);

trellis.par.set(theme = col.fastR(bw = TRUE));
matplot(seq(0.001, 0.999, by = 0.001), tmp[, c(1, 8)], type = "l",
    lty = 1, col = trellis.par.get("superpose.line")$col[1:4],
    main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
    xlab = expression(pi),
    ylab = "Coverage Rate",
    lwd = 2,
    ylim = c(0.8, 1));
    abline(h = 0.95);
    legend(0.40, 0.875, c("Score", "Wilson"), col = trellis.par.get("superpose.line")$col[1:2], lty = 1, cex = 1);
}


xyplot(coverage ~ pi, data = coverage, groups = method,
	lty = 1, lwd = 2, alpha = 0.8,
	type = "l", cex = .25,
    main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
    xlab = expression(pi),
    ylab = "Coverage Rate",
    ylim = c(0.8, 1),
	# col = c("gray50", "gray80", "gray20"),
	col = c("navy", "red", "forestgreen"), #  "purple"),
	auto.key = TRUE,
	legend = list(
		inside= list(x = .5, y = .1, corner = c(.5, 0), 
			fun = draw.key,
			args = list(
				key = list(
					lines = list(lty = 1, lwd = 2,
						# col = c("gray70", "gray20", "gray50")
	          col = c("red", "forestgreen", "navy") 
					),
					text = list(
						lab = c("Clopper-Pearson", "Score", "Wald"),
						cex = .8)
				)
			)
		)
	),
	panel = function(x, y, ...){
		panel.abline(h = 0.95)
		panel.xyplot(x, y, ...)
		}
	);
#write.csv(coverage, file = "CIcoverage.csv", row.names = FALSE)

## ----binom-cis-----------------------------------------------------------
binom.test(25, 70)  # Clopper-Pearson
binom.test(25, 70, ci.method = "Wald")
binom.test(25, 70, ci.method = "score")
 prop.test(25, 70)  # also uses inverted score test

## ----prop-ci-sim-old, include = FALSE, cache = TRUE----------------------
easyci <- function(x, n, conf.level = 0.95) { 
    alpha = 1 - conf.level
    p = x / n
    zstar <- - qnorm(alpha / 2)
    interval <- c(p + c(-1, 1) * zstar * sqrt(p * (1 - p) / n), conf.level)
	names(interval) <- c("lower", "upper", "conf.level")
	interval
    }
Wilsonci <- function(x, n = 100, conf.level = 0.95) { 
    alpha = 1 - conf.level
    p = (x + 2) / (n + 4)
    zstar <- - qnorm(alpha / 2)
    interval <- c(p + c(-1, 1) * zstar * sqrt(p * (1-p) / n), conf.level)
	names(interval) <- c("lower", "upper", "conf.level")
	interval
    }

# sample size = 35
do(1) * easyci(rbinom(1, 35, 0.3), n = 35)    
results <- do(10000) * easyci(rbinom(1, 35, 0.3), n = 35) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data = results)    # coverage rate

results <- do(10000) * Wilsonci(rbinom(1,35,0.3), n = 35) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data = results)    # coverage rate

# sample size = 100 
results <- do(10000) * easyci(rbinom(1,100,0.3), n = 100) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data = results)    # coverage rate

results <- do(10000) * Wilsonci(rbinom(1,100,0.3),n = 100)# simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data = results)    # coverage rate

## ----prop-ci-sim---------------------------------------------------------
Sims <-
  expand.grid(
  n = c(35, 100), 
  pi = c(0.2, 0.3, 0.4, 0.5),
  method = c("Wald", "Wilson", "score"), 
  rep = 1:2000) %>%
  group_by(n, pi, method, rep) %>% 
  do(confint(binom.test(rbinom(1, .$n, .$pi), n = .$n, ci.method = .$method)))

Sims %>%
  group_by(n, pi, method) %>% 
  summarise(cover = prop(lower <= pi & pi <= upper))

## ----helium-football-----------------------------------------------------
Footballs <- HeliumFootballs %>% mutate(diff = helium - air)
Footballs %>% head(3)
t.test( ~ diff, data = Footballs)

## ----helium-football-sign------------------------------------------------
binom.test( ~ (helium > air), data = HeliumFootballs)

## ----buger-barn2a-sol----------------------------------------------------
Sims <-
  expand.grid(n=c(5, 10, 20, 50), rate = 1/10, rep = 1:2000) %>%
  group_by(n, rate, rep) %>%
  mutate(
    S = sum(rexp(n, rate)),
    U = S * rate,
    pvalS = 1 - pgamma(S, shape = n, rate = rate),
    pvalU = 1 - pgamma(U, shape = n, rate = 1)
  )
# We get the same p-value using either S or U as the test stat
xyplot(pvalS ~ pvalU, data = Sims, type = "l")

# The p-values exhibit the expected Unif(0,1) distribution
histogram( ~ pvalU | paste0("n=", n), data = Sims, width = 0.05, xlim = c(0,1))
qqmath( ~ pvalU | paste0("n=", n), data = Sims, dist = qunif)

# S, U approx Normal when sample size is large enough.
histogram( ~ U | paste0("n=", n), data = Sims)
histogram( ~ S | paste0("n=", n), data = Sims)

## ----buger-barn2b-sol----------------------------------------------------
eci <- function(x, conf.level = 0.95, type = c("S", "U")) {
  type <- match.arg(type)
  pv <- 
    switch(
      type,
      U = function(x, rate) {pgamma(sum(x) * rate, shape = length(x), rate = 1)},
      S = function(x, rate) {pgamma(sum(x), shape = length(x), rate = rate)}
    )
  alpha <- (1 - conf.level) / 2             
  list(
    type = type,
    estimate = 1 / mean(x),
    conf.int = 
      c(
        uniroot(function(rate) pv(x, rate) - alpha,       c(0.001, 1000))$root,
        uniroot(function(rate) pv(x, rate) - (1 - alpha), c(0.001, 1000))$root
      )
  )
}
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 1/10, method = eci, method.args = list(type = "U"))
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 1/10, method = eci, method.args = list(type = "S"))

## ----buger-barn2c-sol----------------------------------------------------
eci2 <- function(x, conf.level = 0.95, type = c("S", "U")) {
  type <- match.arg(type)
  pv <- 
    switch(
      type,
      U = function(x, mu) {pgamma(sum(x) / mu, shape = length(x), rate = 1)},
      S = function(x, mu) {pgamma(sum(x), shape = length(x), rate = 1/mu)}
    )
               
  list(
    type = type,
    estimate = mean(x),
    conf.int = 
      c(
        uniroot(function(mu) pv(x, mu) -     conf.level, c(0.01, 100))$root,
        uniroot(function(mu) pv(x, mu) - 1 + conf.level, c(0.01, 100))$root
      )
  )
}
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 10, method = eci2, method.args = list(type = "U"))
CIsim(samples = 1000, n = 5, rdist = rexp, args = list(rate = 1/10), 
      estimand = 10, method = eci2, method.args = list(type = "S"))

## ----endurance-paired----------------------------------------------------
t.test( ~ (vitamin - placebo), data = Endurance)
t.test( ~ (log(vitamin) - log(placebo)) , data = Endurance)
t.test( ~ (log(vitamin / placebo)) , data = Endurance)  # same as above
t.test( ~ (vitamin / placebo), data = Endurance)
t.test(~ (1 / vitamin - 1 / placebo), data = Endurance)
binom.test(~ (vitamin > placebo), data = Endurance)
prop.test(~ (vitamin > placebo), data = Endurance)

## ----Joes-coin-----------------------------------------------------------
wald.ci <- function(x, n, level = 0.95) {
    alpha = 1 - level
    pi.hat <- x / n
    se <- sqrt(pi.hat * (1 - pi.hat) / n)
    z.star <- qnorm(1 - alpha / 2)
    pi.hat + c(-1, 1) * z.star * se
}

## ----Joes-coin2----------------------------------------------------------
wilson.ci<- function(x, n, level = 0.95) {
    x = x + 2; n = n + 4
    alpha = 1 - level
    pi.hat <- x / n
    se <- sqrt(pi.hat * (1 - pi.hat) / n)
    z.star <- qnorm(1 - alpha / 2)
    pi.hat + c(-1, 1) * z.star * se
}

## ----Joes-coin3----------------------------------------------------------
score.ci <- function(x, n, level = 0.95) {
    alpha = 1 - level
    z.star <- qnorm(1 - alpha / 2)
    pi.hat <- x / n
    A <- pi.hat + z.star^2 / (2 * n)
    B <- z.star * sqrt((pi.hat * (1 - pi.hat) / n) 
	                  + (z.star^2 / (4 * n^2)))
    D <- 1 + z.star^2 / n
    # interval is (A +- B) / D
    (A + c(-1, 1) * B) / D
}

## ----Joes-coin4----------------------------------------------------------
prop.test(115, 200)$conf.int
confint(prop.test(115, 200, correct = FALSE))

## ----Joes-coin5----------------------------------------------------------
wald.ci(115, 200)
wilson.ci(115, 200)
wald.ci(117, 204)
score.ci(115, 200)

## ----Joes-coin6----------------------------------------------------------
# score interval using uniroot:
p.hat <- 115 / 200; n <- 200
f <- function(p) {
    abs(p.hat - p) / sqrt(p * (1-p) / n) + qnorm(0.025)
}
uniroot(f, c(0, p.hat))$root
uniroot(f, c(p.hat, 1))$root
uniroot(f, c(0, p.hat))$estim.prec

## ------------------------------------------------------------------------
t.test( ~ (kiln - reg), data = Corn)

## ----golfballs-range, fig.keep = "none"----------------------------------
stat <- function(x) { diff(range(x)) }  
statTally(golfballs, rgolfballs, stat,
	      xlab = "test statistic (range)")

## ----golfballs-range-fig, echo = FALSE, message = FALSE, results = "hide"----
stat <- function(x) { diff(range(x)) }  
statTally(golfballs, rgolfballs, stat,
	      xlab = "test statistic (range)")

## ------------------------------------------------------------------------
rmultinom(1, prob = c(.3, .3, .2, .2), size = 486)
tally(resample(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4), 486))

## ----fisher-twins-perm, cache = TRUE, seed = 123-------------------------
numSims <- 20000 
FT <- 
  data.frame(
    twin = rep(c("Di", "Mono"), times = c(17, 13)),
    conviction = rep(c("No", "Yes", "No", "Yes"), times = c(15, 2, 3, 10))
  )
# check to see that table matches
tally(twin ~ conviction, data = FT)
# test statistic is value in top left cell
tally(twin ~ conviction, data = FT)[1, 1]

# simulated data sets
testStats <- replicate(numSims, {
    tally(twin ~ shuffle(conviction), data = FT)[1, 1]
    })

# for p-value 
tally(testStats)
# tail probabilities
prop1(testStats >= 15)
# 2-sided p-value
2 * prop1(testStats >= 15)

## ----fisher-twins-2sided-------------------------------------------------
prop(testStats >= 15 | testStats <= 5) 

## ----fisher-twins-exact--------------------------------------------------
fisher.test(tally(twin ~ conviction, data = FT)) 

## ----fisher-twins-ci-----------------------------------------------------
binom.test(2 * sum(testStats >= 15), numSims) %>% confint()  
binom.test(sum(testStats >= 15 | testStats <= 5), numSims) %>% confint()  

## ----iris-perm, cache = TRUE---------------------------------------------
data(iris)
Setosa <- iris %>% filter(Species == "setosa")
corStat <- function(x, y) {sum(x * y) - length(x) * mean(x) * mean(y)}

testStat <- with(Setosa, corStat(Sepal.Length, Petal.Length)); testStat
SetosaSims <-
  expand.grid(rep = 1:10000) %>%
  group_by(rep) %>%
  mutate(
    simStat = with(Setosa, corStat(Sepal.Length, shuffle(Petal.Length)))
  )
histogram( ~ simStat, data = SetosaSims, v = testStat)
# 1-sided p-value
prop1( ~ (simStat >= testStat), data = SetosaSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = SetosaSims)

## ----iris-perm-versi, cache = TRUE---------------------------------------
Versi <- iris %>% filter(Species == "versicolor")
testStat <- with(Versi, corStat(Sepal.Length, Petal.Length)); testStat
VersiSims <-
  expand.grid(rep = 1:10000) %>%
  group_by(rep) %>%
  mutate(simStat = with(Versi, corStat(Sepal.Length, shuffle(Petal.Length))))

# 1-sided p-value
prop1( ~ (simStat >= testStat), data = VersiSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = VersiSims)

## ----dimes-boot01--------------------------------------------------------
x.bar <- mean( ~ mass, data = Dimes); x.bar
Dimes.boot <- do(5000) * mean( ~ mass, data = resample(Dimes))
histogram( ~ mean, data = Dimes.boot)

## ----dimes-boot02--------------------------------------------------------
# normality check
xqqmath( ~ mean, data = Dimes.boot)
SE <- sd( ~ mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

## ----dimes-boot03--------------------------------------------------------
cdata( ~ mean, data = Dimes.boot)

## ----dimes-ci------------------------------------------------------------
t.test( ~ mass, data = Dimes) %>% confint()

## ----dimes, include = FALSE, tidy = FALSE--------------------------------
s <- sd(~ mass, data = Dimes)
n <- nrow(Dimes)
B <- 10200; B
D <- mean( ~ mass, data = Dimes); D
uB <- 100 / sqrt(12); uB
uD <- s / sqrt(n); uD
u <- sqrt( 1/D^2 * uB^2 + B^2/D^4 * uD^2  )

## ----propagation-unif1, cache=TRUE---------------------------------------
X <- runif(100000, 0, 1)
Y <- sqrt(X)
mean(Y)
var(Y)

## ----warning = FALSE-----------------------------------------------------
integrate(makeFun( y * 2*y ~ y), 0, 1)
mu <- integrate(function(y) y * 2*y, 0, 1) %>% value()
fractions(mu)
integrate(function(y) (y-mu)^2 * 2*y, 0, 1)
integrate(function(y) (y-mu)^2 * 2*y, 0, 1) %>% value() %>% fractions()

## ----mean-dime-cl--------------------------------------------------------
pt(1, df = 29) - pt(-1, df = 29)

## ----dimes-sim,cache=TRUE, digits = 4------------------------------------
B <- runif(10000, 10150, 10250)
Dimes.boot <- do(10000) * mean( ~ mass, data = resample(Dimes))
head(Dimes.boot, 3)
Dimes.boot <- 
  Dimes.boot %>% mutate(D = mean, N = B / D)
histogram( ~ N, data = Dimes.boot)
qqmath( ~ N, data = Dimes.boot)
sd( ~ N, data = Dimes.boot)

## ----resistors,include=FALSE---------------------------------------------
R <- 20*50/(20 + 50)
u1 <- 0.7; u2 <- 1.2
p1 <- (50/70)^2; p2 <- (20/70)^2
u <- sqrt( p1^2 * u1^2 + p2^2 *u2^2 )
r <- 1 + round(-log10(u))

## ----include=FALSE-------------------------------------------------------
rm(pi)

## ----tidy=FALSE----------------------------------------------------------
L <- 2.65; W <- 3.10; H <- 4.61
uL <- 0.02; uW <- 0.02; uH <- 0.05
V <- L * W * H; V
uV <- sqrt( (uL/L)^2 + (uW/W)^2 + (uH/H)^2) * V; uV

## ----tidy=FALSE----------------------------------------------------------
V <- 1.637 / 0.43; V
uV <- sqrt( (0.02/.43)^2 + (0.006/1.637)^2 ) * V; uV

## ----moments-binom-------------------------------------------------------
x <- 0:20
sum(x * dbinom(x, 20, 0.25))
sum(x^2 * dbinom(x, 20, 0.25))
sum(x^2 * dbinom(x, 20, 0.25)) - (sum(x * dbinom(x, 20, 0.25)))^2

## ----moments-exp---------------------------------------------------------
f1 <- function(x) { x * dexp(x, rate = 2) }
f2 <- function(x) { x^2 * dexp(x, rate = 2) }
integrate(f1, 0, Inf)
integrate(f2, 0, Inf)

## ----test-coin-review----------------------------------------------------
binom.test(60, 100)
prop.test(60, 100)

## ----test-coin-power01---------------------------------------------------
binom.test(61, 100)

## ----test-coin-power02---------------------------------------------------
prob <- c(seq(0, 0.4, by = 0.10), 0.45, 0.5, 0.55, seq(0.6, 1, by = 0.10))
power <- pbinom(39, 100, prob) + 1- pbinom(60, 100, prob)
print(cbind(prob, power))

## ----test-coin-power03, opts.label = "fig1"------------------------------
prob <- seq(0, 1, by = 0.01)
power <- pbinom(39, 100, prob) + 1 - pbinom(60, 100, prob)
xyplot(power ~ prob, type = "l", 
            main = "Power to detect a biased coin with 100 flips",
            xlab = "true probability of heads")

## ----min-of-unif01-------------------------------------------------------
n <- round((1:10)^(1.75)); prob <- 1 - (0.95)^n
cbind(n, prob)

## ----min-of-unif02, opts.label = "fig1"----------------------------------
y <- seq(0, 1, by = 0.01)
n <- c(1, 5, 10, 20)
n <- rep(n, times = length(y))
y <- rep(y, each = 4)

density <-  n * (1 - y)^{n-1}
groups <- paste("n =", n)
groups <- factor(groups, levels = unique(groups))
xyplot(density ~ y, groups = groups, type = "l", 
            main = "Pdf of the mininum of a sample from Unif(0,1)",
            key = simpleKey(levels(groups), columns = 2, lines = TRUE, points = FALSE),
            xlim = c(0, 0.20))

## ----mix-normals01-------------------------------------------------------
0.3 * pnorm(12, 8, 2) + 0.7 * pnorm(12, 16, 3)

## ----mix-normals02, opts.label = "fig1"----------------------------------
x <- seq(0, 30, by = 0.25)
density <- 0.3 * dnorm(x, 8, 2) + 0.7 * dnorm(x, 16, 3)
xyplot(density ~ x, type = "l", 
    main = "pdf of a mixture of normals"
    )

## ----lognormal, opts.label = "fig1"--------------------------------------
#note: these functions do not check for negative values 
#       where they shouldn't be
dlognormal <- function(x, mu = 0, sigma = 1) {
    dnorm(log(x), mean = mu, sd = sigma) * 1 / x
}
rlognormal <- function(n, mu = 0, sigma = 1) {
    normals <- rnorm(n, mean = mu, sd = sigma)
    return(exp(normals))
}
plognormal <- function(x, mu = 0, sigma = 1) {
    pnorm(log(x), mean = mu, sd = sigma) 
}
qlognormal <- function(p, mu = 0, sigma = 1) {
    exp(qnorm(p, mean = mu, sd = sigma))
}
# some checks
randomData <- rlognormal(100, mu = 0, sigma = 1/2)
quant <- quantile(randomData)
x <- qlognormal(c(0.25, 0.5, 0.75), mu = 0, sigma = 1/2); x
plognormal(x, mu = 0, sigma = 1/2)
plognormal(quant, mu = 0, sigma = 1/2)

plot1 <- histogram( ~ randomData)

x <- seq(0, 10, by = 0.25)
nx <- length(x)
mu <- c(-1, 0, 1)
nmu <- length(mu)
sigma <- c(1/8, 1/4, 1/2, 1, 2, 4)
nsigma <- length(sigma)

x <- rep(x, each = nmu * nsigma)
mu <- rep(rep(mu, nsigma), times = nx)
sigma <- rep(rep(sigma, each = nmu), times = nx)
density <- dlognormal(x, mu, sigma)

xyplot(density ~ x | paste("sigma", "=", sigma), 
                groups = paste("mu =", mu), 
                type = "l", 
                key = simpleKey(paste("mu =", sort(unique(mu))),
                        points = FALSE, lines = TRUE, columns = 3),
                scales = list(y = list(relation = "free", alternating = FALSE)),
                main = "pdfs of lognormal distributions")

## ----review-faithful-----------------------------------------------------
t.test( ~ eruptions, data = faithful)

## ----moments-function----------------------------------------------------
moment <- function(
    k = 1,                                     # which moment
    vals = 1:6,                                # dice by default
    probs = rep(1 / length(vals), length(vals)), # uniform probs
    centered = FALSE) {                        # center on mean of data?

    if (length(k) > 1) {  # vectorize this (fancy)
        return(sapply(k, moment, vals = vals, probs = probs, centered = centered))
    }

    if (centered) {
        m = sum(vals * probs)
    } else { 
        m = 0
    }
    sum((vals-m)^k * probs)
}

moment(k = 1, 0:10, dbinom(0:10, 10, 0.4))
moment(k = 2, 0:10, dbinom(0:10, 10, 0.4), centered = FALSE)
moment(k = 2, 0:10, dbinom(0:10, 10, 0.4), centered = TRUE)
10 * 0.4 * 0.6   # should match previous and next value
moment(k = 2, 0:10, dbinom(0:10, 10, 0.4), centered = FALSE) - 
    moment(k = 1, 0:10, dbinom(0:10, 10, 0.4), centered = FALSE)^2
round(moment(k = 1:4, 0:10, dbinom(0:10, 10, 0.4), centered = FALSE), 5)
round(moment(k = 1:4, 0:10, dbinom(0:10, 10, 0.4), centered = TRUE), 5)

## ----moments-function02--------------------------------------------------
moment.cont <- function(k = 1,       # which moment?
        dist = dnorm,  
        args = list(),                # arguments to dist()
        range = c(-Inf, Inf),          
        centered = FALSE) {           # centered on mean?

    if (length(k) > 1) {  # vectorize this (fancy)
        return(
          sapply(k, moment.cont, dist = dist, 
                 args = args, range = range, centered = centered))
    }

    if (centered) {
        m = moment.cont(dist = dist, range = range, k = 1, centered = FALSE)
    } else { 
        m = 0
    }
    int.out <- integrate(
                    function(x) { (x - m)^k * dist(x) }, 
                    range[1], range[2])
    return (int.out$value)
}

moment.cont(dunif, k = 1, centered = FALSE)
moment.cont(dunif, k = 2, centered = FALSE)
moment.cont(dunif, k = 2, centered = TRUE)
moment.cont(dunif, k = 1:4, centered = FALSE)
round(moment.cont(dunif, k = 1:4, centered = TRUE), 5)
round(moment.cont(dnorm, k = 1:4, centered = TRUE), 5)
round(moment.cont(function(x) {dnorm(x, 10, 3)}, k = 1:4, centered = TRUE), 5)


## ----Likelihood, child="Likelihood.Rnw", eval=includeChapter[5]----------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Lik-") 
require(maxLik)

## ----dice-likelihood-sol-------------------------------------------------
plotDist("binom", size = 10, prob = 1/2, ylim = c(0, 0.35), type = c("p", "l"))
plotDist("binom", size = 10, prob = 1/3, add = TRUE, col = "red", type = c("p", "l"))
plotDist("binom", size = 10, prob = 1/5, add = TRUE, col = "green", type = c("p", "l"))

## ----dice-10-sol---------------------------------------------------------
pbinom(2, 10, 1/5)

## ----dice-others-sol-----------------------------------------------------
pbinom(4, 10, 1/3) - pbinom(2, 10, 1/3)
1 - pbinom(4, 10, 1/2)

## ----binom-mle-fig, echo=FALSE-------------------------------------------
llik <- function (p, t = 14) {
    14*log(p) + 26 * log(1-p)
}
xpts <- seq(0, 1, by = 0.005)
xyplot(
  llik(xpts) ~ xpts,
  type = "l", lwd = 2, 
  xlab = expression(pi),
  ylab = "log-likelihood"
)

## ----zero-one-mom-mle-sol------------------------------------------------
x <- c(0.90, 0.78, 0.93, 0.64, 0.45, 0.85, 0.75, 0.93, 0.98, 0.78)
mean(x)
mom <- (1 / (1 - mean(x))) - 2; mom
mle <- ( - length(x) / sum(log(x))) - 1; mle

## ----plant-density-sol01, cache=TRUE-------------------------------------
# plant densities (lambda) for simulations
density <- c(0.01, 0.1, 0.25, 0.5, 1, 2, 4, 10, 100)       
sizes <- c(10, 20, 50)
simulate <- function(lambda = 1, size = 10, area = 1, method = c("count", "distance")){
  method <- match.arg(method) # allows for prefixes
  if (method == "count") {
    x <- rpois(size, lambda*area)
    plants <- sum(x)
    total.area <- size * area
    mle <- plants / total.area  
  } else {
    y <- rweibull(size, shape = 2, scale = 1 / sqrt(pi * lambda))
    plants <- length(y)
    total.area <- pi * sum(y^2)
    mle <- plants / total.area  
  }
  data.frame(
    size = size, lambda = lambda, method = method,
    estimate = mle, plants = plants, area = total.area,
    lambdaFac = paste0("l=", lambda),
    sizeFac = paste0("size=", size)
  )
}

## ----plant-density-sol02, cache=TRUE, tidy=FALSE-------------------------
Results <- c()
for (lambda in density) {
  for (size in sizes) {
    Results <- bind_rows(Results, 
	  do(1000) * simulate(lambda, size, method = "count"), 
	  do(1000) * simulate(lambda, size, method = "distance") 
	) 
  }
}

## ----plant-density-bakeoff-sol, tidy=FALSE-------------------------------
# have a bake-off:
SummaryResults <- 
  Results %>%
  group_by(size, lambda, method) %>%
  summarise(
    bias = mean(estimate) - lambda[1],
    biasR = mean(estimate) / lambda[1],
    se = sd(estimate)
  )
head(SummaryResults)

## ----eval=FALSE----------------------------------------------------------
## log(estimate / lambda)

## ----plant-density-plot-sol01, fig.height=8, tidy=FALSE------------------
latticeExtra::useOuterStrips(
  stripplot(method ~ log(estimate / lambda) | sizeFac + lambdaFac, 
            data = Results, jitter = TRUE, alpha = .1, as.table = TRUE)
)

## ----plant-density-plot-sol02, fig.height=8, tidy=FALSE, warning=FALSE----
ggplot(data = Results, 
  aes(y = log(estimate / lambda), x = method, colour = method, fill = method)) +
  geom_violin(alpha = 0.5) +
  annotate("hline", yintercept = 0) + coord_flip() +  
  facet_grid(lambdaFac ~ sizeFac) + theme_bw()

latticeExtra::useOuterStrips(
  bwplot(method ~ log(estimate / lambda) | sizeFac * lambdaFac,
    data = Results, 
    groups = method,
    panel = panel.violin,
    auto.key = TRUE, as.table = TRUE)
)

## ----rmultinom-sol-------------------------------------------------------
rmultinom2 <- function(n, size, prob) {
	prob <- prob / sum(prob)
	x <- runif(n * size, 0, 1)
	y <- as.numeric(cut(x, c(0, cumsum(prob))))
  y <- factor(y, levels = 1:length(prob))    # so we get 0 counts recorded
  M <- matrix(y, nrow = n)                     # put results into a matrix
  apply(M, 1, table)                         # create table for each row
}
rmultinom2(4, 100, c(.1, .2, .3, .4))

## ----baseballBA01, tidy = FALSE------------------------------------------
ba <- c(0.320, 0.297, 0.264, 0.306, 0.291, 0.290, 0.316, 
        0.324, 0.295, 0.294, 0.235, 0.283, 0.273, 0.295, 0.327)

## ----baseballBA01-fig, echo=FALSE----------------------------------------
plotDist("beta", shape1= 107.1, shape2 = 257.3)

## ----baseballBA02, tidy = FALSE------------------------------------------
# log likelihood function
loglik <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    sum(dbeta(x, theta[1], theta[2], log = TRUE)) 
}
# alternative way to define the log-likelihood
loglik2 <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    dbeta(x, theta[1], theta[2], log = TRUE) 
}

## ----baseballBA03--------------------------------------------------------
require(maxLik)
ml  <- maxLik(loglik,  start = c(shape1 = 1, shape2 = 1), x = ba)
ml2 <- maxLik(loglik2, start = c(shape1 = 1, shape2 = 1), x = ba)
ml
ml2
# get just the estimated parameter values
coef(ml)
# get just the "return message" -- always good to check
returnMessage(ml)

## ----baseballBA04--------------------------------------------------------
# using the ruler method
qbeta(313.5 / 314, 107, 257)
qbeta(0.5 / 314, 107, 257)

## ----baseballBA-likelihood-fig, echo = FALSE, include=FALSE--------------
dat <- expand.grid(
	alpha = seq(4, 210, by = 6),
	beta = seq(10, 500, by = 15)
	)

dat$loglik <- apply(cbind(dat$alpha, dat$beta), 1, FUN = "loglik", x = ba)

wireframe(
  exp(loglik) ~ alpha * beta, dat, 
  col = "gray25",
  par.settings = list(
    box.3d = list(col = "transparent"),
    axis.line = list(col = NA, lty = 1, lwd = 1)
  ),
  shade = FALSE, 
  light.source = c(25, 50, 50),
  aspect = c(1, 0.4),
  screen = list(z = 20, x = -75),
  xlab = expression(alpha),
  ylab = expression(beta),
  zlab = "",
  scale = list(arrows = FALSE, z = list(draw = FALSE))
)

dat <- expand.grid(
	alpha = seq(4, 325, by = 1),
	beta = seq(10, 800, by = 2)
	) 

dat$loglik <- apply(cbind(dat$alpha, dat$beta), 1, FUN = "loglik", x = ba)

levelplot(loglik ~ alpha + beta, data = dat,
          xlab = expression(alpha),
          ylab = expression(beta),
          main = "log-likelihood", 
          col.regions = topo.colors(n=100)
)

## ----normal-loglik-------------------------------------------------------
loglik.normal <- function(theta, x) {
  mu <- theta[1]; sigma <- theta[2]
  if (sigma < 0) return(NA)     # alert maxLik() to invalid values of sigma
  dnorm(x, mu, sigma, log = TRUE)
}

## ----normal-mle01--------------------------------------------------------
x <- rnorm(40, 100, 10)
maxLik(loglik.normal, start = c(mu = 0, sigma = 1), x = x)

## ----normal-mle02--------------------------------------------------------
MLEs <-
  do(5000) * coef(maxLik(loglik.normal, 
                         start = c(mu = 0, sigma = 1), x = rnorm(40, 100, 10)))
head(MLEs, 3)
histogram( ~ mu,    data = MLEs, width = 0.5, xlab = expression(hat(mu)))
histogram( ~ sigma, data = MLEs, width = 0.5, xlab = expression(hat(sigma)))
xqqmath( ~ mu,    data = MLEs, ylab = expression(hat(mu)))
xqqmath( ~ sigma, data = MLEs, ylab = expression(hat(sigma)))

## ----normal-mle03--------------------------------------------------------
histogram( ~ sigma^2, data = MLEs, width = 5)
xqqmath( ~ sigma^2, data = MLEs, 
         distribution = function(p) qchisq(p, df = 39),
         xlab = "Chisq(39)", ylab = expression(hat(sigma)^2))

## ----faithful-mle01, fig.show="hide", tidy=FALSE-------------------------
# density function for mixture of normals
dmix <- function(x, alpha, mu1, mu2, sigma1, sigma2) {
  if (alpha < 0 || alpha > 1) return (NA)
  if (sigma1 < 0 || sigma2 < 0) return (NA)
  alpha * dnorm(x, mu1, sigma1) + (1-alpha) * dnorm(x, mu2, sigma2)
}

## ----faithful-mle02------------------------------------------------------
# log-likelihood
loglik.faithful <- function(theta, x) {
  alpha <- theta[1]  
  mu1 <- theta[2]; mu2 <- theta[3]
  sigma1 <- theta[4]; sigma2 <- theta[5]
  
  sum(log(dmix(x, alpha, mu1, mu2, sigma1, sigma2)))
}

## ----faithful-mle03, fig.show="hide", tidy=FALSE-------------------------
# seed the algorithm  
data(geyser, package = "MASS")
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml.faithful <- 
  maxLik(loglik.faithful, x = geyser$duration,
    start = c(alpha = 0.5, mu1 = m - 1, mu2 = m + 1, sigma1 = s, sigma2 = s)) 
returnMessage(ml.faithful)
mle <- coef(ml.faithful); mle

histogram( ~ duration, data = geyser,
           width = 0.25,
           density = TRUE,
           dmath = dmix,
           args = list(
             alpha =  mle[1],
             mu1 =    mle[2], mu2 =    mle[3],
             sigma1 = mle[4], sigma2 = mle[5])
)

## ----faithful-mle03-fig, echo = FALSE, results = "hide"------------------
# seed the algorithm  
data(geyser, package = "MASS")
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml.faithful <- 
  maxLik(loglik.faithful, x = geyser$duration,
    start = c(alpha = 0.5, mu1 = m - 1, mu2 = m + 1, sigma1 = s, sigma2 = s)) 
returnMessage(ml.faithful)
mle <- coef(ml.faithful); mle

histogram( ~ duration, data = geyser,
           width = 0.25,
           density = TRUE,
           dmath = dmix,
           args = list(
             alpha =  mle[1],
             mu1 =    mle[2], mu2 =    mle[3],
             sigma1 = mle[4], sigma2 = mle[5])
)

## ----faithful-mle04, tidy = FALSE----------------------------------------
# seed the algorithm  
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

# Newton-Raphson (NR) compares well to the results above
maxLik(loglik.faithful,  x = geyser$duration,
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s)) 
# Neler-Mead doesn't converge (fast enough)
maxLik(loglik.faithful, x = geyser$duration, method = "NM",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s)) 
# Nelder-Mead converges if we give it more time
maxLik(loglik.faithful, x = geyser$duration, method = "NM", 
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s), 
       control = list(iterlim = 3000))
# BFGS "converges", but only fits one group
maxLik(loglik.faithful, x = geyser$duration, method = "BFGS",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s))

## ----multinom-mle--------------------------------------------------------
loglik.multinom <- function(theta, x) { 
  probs <- c(theta, 1 - sum(theta))
  if (any (probs < 0)) return(NA)
  dmultinom(x, size = 100, prob = probs, log = TRUE)
}
maxLik(loglik.multinom, start = rep(0.25, 3), x = c(10, 20, 30, 40)) -> ml; 
coef(ml)

## ----unif-mle-sol01, warning=FALSE---------------------------------------
x <- c(1.6, 2.8, 6.2, 8.2, 8.7)
loglik.unif <- function(theta, x) {
  res <- sum (dunif(x, 0, theta, log = TRUE))
  ifelse(is.finite(res), res, NA)
}

lik.unif <- function(theta, x) {
  res <- prod (dunif(x, 0, theta, log = FALSE))
  ifelse(is.finite(res), res, NA)
}

## ----unif-mle-sol02, warning=FALSE---------------------------------------
# works if we select a good starting point -- but warns about boundary issues.
maxLik(loglik.unif, start = 10, x = x)
maxLik(loglik.unif, start = 10, x = x, method = "NM")
maxLik(loglik.unif, start = 10, x = x, method = "BFGS")
# but some starting points don't work well...
maxLik(loglik.unif, start = 8, x = x)
maxLik(loglik.unif, start = 8, x = x, method = "NM")
maxLik(loglik.unif, start = 8, x = x, method = "BFGS")

## ----unif-mle-sol03, warning = FALSE-------------------------------------
# a graph of the likelihood function shows why
theta <- seq(6, 12, by = 0.002)
y1 <- sapply(theta, function(theta) { lik.unif(theta, x)} )
y2 <- sapply(theta, function(theta) { loglik.unif(theta, x)} )
xyplot(y1 ~ theta,
    xlab = expression(theta),
    ylab = "likelihood", cex = 0.5)
xyplot(y2 ~ theta,
    xlab = expression(theta),
    ylab = "log-likelihood", cex = 0.5)

## ----hwe-mle-------------------------------------------------------------
theta2probs <- function(theta) { 
    c(theta^2, 2*theta*(1-theta), (1-theta)^2)  
}
loglik.hwe <- function(theta, x) {
  probs <- theta2probs(theta)
  if (any(probs < 0 )) { return(NA) }
	dmultinom(x, sum(x), theta2probs(theta), log = TRUE)
}

geno<-c(83, 447, 470)
maxLik(loglik.hwe, start = 0.5, x = geno)

## ----mix-normals-sol01---------------------------------------------------
0.3 * pnorm(12, 8, 2) + 0.7 * pnorm(12, 16, 3)

## ----mix-normals-sol02---------------------------------------------------
x <- seq(0, 30, by = 0.25)
density <- 0.3 * dnorm(x, 8, 2) + 0.7 * dnorm(x, 16, 3)
xyplot(density ~ x, type = "l", 
    main = "pdf of a mixture of normals"
    )

## ----pois-lrt01, tidy = FALSE--------------------------------------------
x <- c(1, 1, 0, 4, 2, 1, 3, 0, 0, 2);  tally(x)
mean(x) 
lrtStat <- function(x, lambda0 = 1) {
    x.bar <- mean(x); n <- length(x)
    2 * ( - n * x.bar   + n * x.bar * log(x.bar) + 
            n * lambda0 - n * x.bar * log(lambda0))
    }
lrtStat(x)
pval <- 1 - pchisq(lrtStat(x), df = 1); pval

## ----pois-lrt02, fig.keep = "none"---------------------------------------
# We can express l() in terms of sufficient statistics 
loglik.pois <- function(theta, x.bar = 1.4, n = 10) {
  - n * theta + n * x.bar * log(theta)
}
ml.pois10 <- 
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 10)
plot(ml.pois10) + labs(title = "n = 10")

## ----pois-lrt03----------------------------------------------------------
-hessian(ml.pois10)   # I = - hessian
stdEr(ml.pois10)      # I^(-1/2)(theta.hat)
(-hessian(ml.pois10))^(-1/2)

## ----pois-lrt04, fig.keep = "none"---------------------------------------
ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) + labs(title = "n = 100")

## ----pois-lrt04-fig, echo = FALSE----------------------------------------
# We can express l() in terms of sufficient statistics 
loglik.pois <- function(theta, x.bar = 1.4, n = 10) {
  - n * theta + n * x.bar * log(theta)
}
ml.pois10 <- 
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 10)
plot(ml.pois10) + labs(title = "n = 10")
ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) + labs(title = "n = 100")

## ----pois-lrt05----------------------------------------------------------
-hessian(ml.pois100)     # information
stdEr(ml.pois100)        

## ----pois-mle, cache=TRUE, seed = 123------------------------------------
# generate 5000 samples of size 10
rdata <- do(5000) * rpois(10, 1)
statTally(x, rdata, lrtStat)

## ----pois-wald-----------------------------------------------------------
SE <- stdEr(ml.pois10); SE
z.star <- qnorm(0.975); z.star
1.4 + c(-1, 1) * z.star * SE

## ----pois-ci, tidy = FALSE-----------------------------------------------
# loglik.pois defined above  
p <- function(t0) {  
  lrt.stat <- 2 * (loglik.pois(coef(ml.pois10)) - loglik.pois(t0)) 
  1 - pchisq(lrt.stat, df = 1) # p-value        
}
lo <- uniroot(function(t){p(t) - 0.05}, c(0,  coef(ml.pois10))) %>% value()
hi <- uniroot(function(t){p(t) - 0.05}, c(10, coef(ml.pois10))) %>% value()
# confidence interval
c(lo, hi)

## ----pois-ci-plot, tidy = FALSE, fig.keep = "none"-----------------------
plot(ml.pois10, ci = c("wald", "li"), hline = TRUE)  

## ----pois-ci-plot-fig, tidy = FALSE, echo = FALSE------------------------
plot(ml.pois10, ci = c("wald", "li"), hline = TRUE)  

## ----binom-waldci--------------------------------------------------------
x <- 35; n <- 55
pi.hat <- x / n; pi.hat
SE <- sqrt(pi.hat * (1 - pi.hat) / n); SE
pi.hat + c(-1, 1) * qnorm(0.975) * SE

## ----binom-lci, tidy = FALSE---------------------------------------------
loglik.binom <-  function(p, x, n) { 
  ifelse (p < 0 | p > 1, NA, x * log(p) + (n-x) * log(1 - p))
}
lo <- uniroot( 
  function(pi0) {
    2 * (loglik.binom(pi.hat, x, n) - loglik.binom(pi0, x, n)) - 
    qchisq(.95, df = 1)}, 
  c(0, pi.hat)) %>% value()
hi <- uniroot( 
  function(pi0) {
    2 * (loglik.binom(pi.hat, x, n) - loglik.binom(pi0, x, n)) - 
    qchisq(.95, df = 1)}, 
  c(pi.hat, 1)) %>% value()
c(lo, hi)

## ----binom-ci-compare-fig, echo = FALSE, warning=FALSE-------------------
ml.binom <- maxLik2(loglik.binom, x = 35, n = 55, start = 0.5) 
plot(ml.binom, ci = c("w", "l"), hline = TRUE) +
  labs(x = expression(pi))

## ----binom-odds-ci, tidy = FALSE-----------------------------------------
loglik.binom2 <- function(theta, x, n) {
  x * log(theta / (1 + theta)) + (n - x) * log(1 / (1 + theta))
}
ml.binom2 <- maxLik2(loglik.binom2, start = (odds = 1), x = 35, n = 55)
coef(ml.binom2)
x <- 35; n <- 55; theta.hat <- 35 / 20; theta.hat
lo2 <- 
  uniroot(
    function(theta0) 
      2 * (loglik.binom2(theta.hat, x, n) - loglik.binom2(theta0, x, n)) - 
      qchisq(.95, df = 1), 
    c(0, theta.hat)) %>%
  value()
hi2 <- 
  uniroot(
    function(theta0) 
      2 * (loglik.binom2(theta.hat, x, n) - loglik.binom2(theta0, x, n)) - 
      qchisq(.95, df = 1), 
    c(theta.hat, 100)) %>%
  value()

c(lo2, hi2)
c(lo2, hi2) / (1 + c(lo2, hi2))
c(lo, hi)    # interval computed previously, for comparison

## ----binom-odds-ci-fig, echo = FALSE, warning = FALSE--------------------
plot(ml.binom2, hline = TRUE) + 
  ylim(-45, -35) +
  labs(x = "log odds")

## ----logodds-sol01-------------------------------------------------------
loglik.binom3 <- function(theta, x, n) {
  x * log(exp(theta) / (1 + exp(theta))) + (n - x) * log(1 / (1 + exp(theta)))
}
ml.binom3 <- maxLik2(loglik.binom3, x = 35, n = 55, start = c(logodds = 0))
logodds.hat <- coef(ml.binom3); logodds.hat
p <- function(logodds) {
  2 * (loglik.binom3(coef(ml.binom3), x=35, n=55) - loglik.binom3(logodds, x=35, n=55))
}
lo <- uniroot(function(logodds) p(logodds) - 0.5, c(-100, logodds.hat)) %>% value()
hi <- uniroot(function(logodds) p(logodds) - 0.5, c(100, logodds.hat)) %>% value()
c(lo, hi)

## ----logodds-sol02, tidy = FALSE, warning=FALSE--------------------------
plot(ml.binom, hline = TRUE) +
  ylim(-42, -35.5) + 
  labs(title = "parameter: proportion")
plot(ml.binom2, hline = TRUE) +
  ylim(-42, -35.5) + 
  labs(title = "parameter: odds")
plot(ml.binom3, hline = TRUE) +
  ylim(-42, -35.5) + 
  labs(title = "parameter: log odds")

## ----logodds-sol03-------------------------------------------------------
pi.hat <- coef(ml.binom); pi.hat
odds.hat <- pi.hat / (1 - pi.hat); odds.hat
coef(ml.binom2)
log(odds.hat)
coef(ml.binom3)

## ----faithful-lrt01------------------------------------------------------
snippet("faithful-mle01", echo = FALSE) 
snippet("faithful-mle02", echo = FALSE)

loglik0.faithful <- function(theta, x) {
    theta <- c(0.5, theta)
    return(loglik.faithful(theta, x))
}

## ----faithful-lrt02, tidy = FALSE----------------------------------------
# seed the algorithm  
m <- mean( ~ duration, data = geyser)
s <- sd( ~ duration, data = geyser)

ml <- maxLik(loglik.faithful, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml); mle
loglik.faithful(mle, x = geyser$duration)
logLik(ml)        # makLik can caclulate this log-likelihood for us

ml0 <- maxLik(loglik0.faithful, x = geyser$duration,
              start = c(m - 1, m + 1, s, s))
mle0 <- coef(ml0); mle0
logLik(ml0)                      
lrt.stat <- 2 * (logLik(ml) - logLik(ml0)); lrt.stat
1 - pchisq(lrt.stat, df = 1)     # p-value based on asymptotic distribution

## ----faithful-lrt03, tidy = FALSE----------------------------------------
ml0a <- maxLik(loglik.faithful, x = geyser$duration,
            start = c(0.5, m - 1, m + 1, s, s),
            fixed = 1)       # first parameter is fixed at start value
coef(ml0a)
logLik(ml0a)

## ----laplace-------------------------------------------------------------
dlaplace <- function(x, theta, lambda) {
    0.5 * lambda * exp(-lambda * abs(x-theta))
}
# two ways to do plaplace:
integrate(function(x) {dlaplace(x, 1, 2)}, -Inf, Inf)     # should = 1
plaplace1 <- function(q, theta = 0, lambda = 1) {
    integrate(function(x) {dlaplace(x, theta, lambda)}, -Inf, q)$value
}

plaplace2 <- function(q, theta = 0, lambda = 1) {
    if (q < theta) return(0.5 * (1-pexp(theta-q, lambda)))
    return(0.5 + 0.5 * pexp(q-theta, lambda))
    }
# should get same results either way:
plaplace1(3, lambda = 2, theta = 1)
plaplace1(3, lambda = 2, theta = 1) - plaplace1(-3, lambda = 2, theta = 1)
plaplace2(3, lambda = 2, theta = 1)
plaplace2(3, lambda = 2, theta = 1) - plaplace2(-3, lambda = 2, theta = 1)

## ----laplace-mle-sol01---------------------------------------------------
x <- c(1.00, -1.43, 0.62, 0.87, -0.66, -0.59, 1.30, -1.23, -1.53, -1.94)
loglik.laplace <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(0.5) + dexp(abs(x-m), rate = lambda, log = TRUE)))
}

## ----laplace-mle-sol02---------------------------------------------------
ml.laplace <- maxLik(loglik.laplace, start = c(0, 1), x = x)
ml.laplace

## ----laplace-mle-sol03---------------------------------------------------
loglik.laplace2 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(dlaplace(x, m, lambda))))
}
ml.laplace2 <- maxLik(loglik.laplace2, start = c(0, 1), x = x)
ml.laplace2

## ----laplace-moments-sol-------------------------------------------------
# method of moments estimates
# estimate for theta is the sample mean since E(X) == est.theta:
est.theta = mean(x); est.theta
# estimate for variance satisfies v == 2 / nest.lambda^2:
n <- length(x)
v <- var(x) * (n-1) / n
est.lambda <- sqrt(2 / v); est.lambda

## ----laplace-lrt01-------------------------------------------------------
# enter data
x <- c(1.00, -1.43, 0.62, 0.87, -0.66, -0.59, 1.30, -1.23, -1.53, -1.94)
loglik.laplace1 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}
loglik.laplace0 <- function(theta, x) {
    m <- 0; lambda <- theta[1]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}

## ----laplace-lrt02-------------------------------------------------------
free <- maxLik(loglik.laplace1, start = c(m = 0, lambda = 1), x = x); free  
free.est <- coef(free)
null <- maxLik(loglik.laplace0, start = c(lambda = 1), x = x); null
null.est <- coef(null)
w <- 2 * (loglik.laplace1(free.est, x) - loglik.laplace0(null.est, x)); w
1 - pchisq(w, df = 1)          # p-value based on asymptotic distribution

## ----include = FALSE-----------------------------------------------------
theta <- 1.8
set.seed(123)
rfoo <- function(n, theta) {runif(n)^(1 / (theta + 1))}
pfoo <- function(p, theta) {
  x^(theta + 1)
}
qfoo <- function(p, theta) {
  p^(1 / (theta + 1))
}

dfoo <- function(x, theta, log = FALSE) {
  if (log) {
    log(theta + 1) + theta * log(x)
  } else {
    (theta + 1) * x^theta
  }
}
x <- round(rfoo(30, theta), 2); x
histogram(~ x)
plotDist("foo", theta = 1.8, add = TRUE)

## ------------------------------------------------------------------------
n <- length(x)
theta.hat <- -sum(log(x)) / n  - 1
W <- 2 * (n * log(theta.hat + 1) + theta.hat * sum(log(x)) )

## ----faithful-ci, tidy = FALSE, warning=FALSE----------------------------
# loglik defined above   
snippet("faithful-mle01", echo = FALSE)
snippet("faithful-mle02", echo = FALSE)
m <- mean( ~ duration, data = geyser)
s <-   sd( ~ duration, data = geyser)
ml.faithful <- maxLik(loglik.faithful, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml.faithful)
f <- function(a) {
  ml.faithful.a <- maxLik(loglik.faithful, x = geyser$duration,
                start = c(a, m - 1, m + 1, s, s), 
                fixed = 1)
  lrt.stat <- 2 * (logLik(ml.faithful) - logLik(ml.faithful.a)) 
  pval <- 1 - pchisq(lrt.stat, df = 1)         
  return(pval)
}
lo <- uniroot(function(a){f(a) - 0.05}, c(0.1, mle[1])) %>% value(); lo
hi <- uniroot(function(a){f(a) - 0.05}, c(0.9, mle[1])) %>% value(); hi

## ----golfballs-max, eval=FALSE, tidy = FALSE-----------------------------
## golfballs <- c(137, 138, 107, 104)
## statTally(golfballs, rgolfballs, max,
##           xlab = "test statistic (max)")

## ----golfballs-max-fig, echo = FALSE, message = FALSE, results = "hide"----
golfballs <- c(137, 138, 107, 104)
statTally(golfballs, rgolfballs, max, 
          xlab = "test statistic (max)")

## ----golfballs-lrt, digits = 4-------------------------------------------
# LRT calculation
o <- golfballs; o  
e <- rep(486 / 4, 4); e
G <- 2 * sum (o * log(o / e)); G       # lrt Goodness of fit statistic
1 - pchisq(G, df = 3)

## ----golfballs-pearson-sim-----------------------------------------------
E <- rep(486 / 4, 4)
chisqstat <- function(x) { sum((x - E)^2 / E) }
statTally(golfballs, rgolfballs, chisqstat, xlab = expression(X^2))

## ----golfballs-pearson---------------------------------------------------
# manual calculation
o <- golfballs; o
e <- rep(486 / 4, 4); e
X <- sum ((o - e)^2 / e); X
1 - pchisq(X, df = 3)
# repeated using built-in method
chisq.test(o)

## ----golfballs-complex---------------------------------------------------
o
a <- sum(o[1:2]) / (2 * sum(o)); a
b <- sum(o[3:4]) / (2 * sum(o)); b
a + b                                # should equal 0.5
lnum <- 275 * log(a) + 211 * log(b)
ldenom <- sum(o * log (o/ sum(o)))
G <- -2 * (lnum - ldenom); G
1 - pchisq(G, df = 2)

## ----fit-bugs-pois01-----------------------------------------------------
o <- c(2, 10, 16, 11, 5, 3, 3)
o.collapsed <- c(2 + 10, 16, 11, 5, 3 + 3)
n <- sum(o)
m <- sum(o * 0:6) / n     # mean count = MLE for lambda (full data)
p <- dpois(0:6, m)  
p.collapsed <- c(p[1] + p[2], p[3:5], 1 - sum(p[1:5]))   # collapsed probs
e.collapsed <- p.collapsed * n
cbind(o.collapsed, p.collapsed, e.collapsed)
lrt  <- 2 * sum(o.collapsed * log(o.collapsed / e.collapsed)); lrt
pearson <- sum((o.collapsed - e.collapsed)^2 / e.collapsed); pearson
1-pchisq(lrt, df = 3)
1-pchisq(pearson, df = 3)

## ----fit-bugs-pois02-----------------------------------------------------
1-pchisq(pearson, df = 5-1)
1-pchisq(pearson, df = 5-1-1)

## ----fit-exp-------------------------------------------------------------
Edata <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
n <- length(Edata)
theta.hat <- 1 / mean(Edata); theta.hat
cutpts <- c(0, 2.5, 6, 12, Inf)
bin.Edata <- cut(Edata, cutpts)
p <- diff(pexp(cutpts, theta.hat))
e <- n * p
o <- tally(bin.Edata)
print(cbind(o, e))
lrt  <- 2 * sum(o * log(o / e)); lrt
pearson <- sum((o - e)^2 / e); pearson
1-pchisq(lrt, df = 2)               # df = (4 - 1) - 1 [anti-conservative]
1-pchisq(pearson, df = 2)
1-pchisq(lrt, df = 3)               # df = 4 - 1       [conservative]
1-pchisq(pearson, df = 3)

## ----GOF-sol01, tidy=FALSE-----------------------------------------------
GOF <- 
  function(
    x, 
    lik = function(theta, x) { 
        return(sum(dnorm(x, mean = theta[1], sd = theta[2], log = TRUE)))
    } ,
    pdist = function(x, theta) { 
        return(pnorm(x, mean = theta[1], sd = theta[2]) ) 
    } , 
    start = c(0, 1), cutpts = quantile(x), 
    paramNames = paste("parameter", 1:length(start)),
    pearson = FALSE, ...) 
{
    ml <- maxLik(lik, start = start, x = x, ...)
    mle <- coef(ml)
    names(mle) <- paramNames
    prob <- diff(pdist(cutpts, mle))
    n <- length(x)
    o <- tally(cut(x, cutpts))
    e <- prob * n


    pearsonStat <- sum((o - e)^2 / e)
    lrtStat <- 2 * sum(o * log(o / e))
    df = length(cutpts) - 2 - 2

    if (pearson) {
        pval <- 1- pchisq(pearsonStat, df = df)
        method= "Pearson Goodness of Fit Test"
        stat = pearsonStat
    } else {
        pval <- 1- pchisq(lrtStat, df = df)
        method= "LRT Goodness of Fit Test"
        stat = lrtStat
    }

    names(df) = "df"
    names(stat) = "X-squared"
    message(returnMessage(ml))

    structure(list(
        nlmax = ml, statistic = stat, 
        estimate = coef(ml), parameter = df, 
        p.value = pval, method = method,
        data.name = deparse(substitute(x)), 
        observed = o, expected = e, 
        residuals = (o - e) / sqrt(e),
        table = cbind(o, e, prob),
        message = returnMessage(ml)
        ), 
        class = "htest")
}

## ----GOF-sol02-----------------------------------------------------------
data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
GOF(data, cutpts = c(0, 3, 5, 9, 13, Inf), iterlim = 1000, 
    start = c(5, 5))$table
GOF(data, cutpts = c(0, 3, 5, 9, 13, Inf), iterlim = 1000, start = c(5, 5))
GOF(data, cutpts = c(0, 3, 5, 9, 13, Inf), iterlim = 1000, start = c(5, 5), 
    pearson = TRUE)

## ----gof-gamma-----------------------------------------------------------
oldopt <- options(warn = -1)
data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
ngamlik <- 
  function(theta, x) { -sum(dgamma(x, theta[1], theta[2], log = TRUE)) }
pgamm <- 
  function(x, theta){ pgamma(x, theta[1], theta[2]) }
GOF(data, ngamlik, pgamm, start = c(1, 1), cutpts = c(0, 3, 5, 9, 13, Inf))
GOF(data, ngamlik, pgamm, start = c(1, 1), cutpts = c(0, 3, 5, 9, 13, Inf), 
    pearson = TRUE)
options(oldopt)

## ----gof-weibull, warning=FALSE------------------------------------------
data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
nweiblik <- 
  function(theta, x) { -sum(dweibull(x, theta[1], theta[2], log = TRUE)) }
pweib <- function(x, theta){ pweibull(x, theta[1], theta[2]) }
GOF(data, nweiblik, pweib, start = c(1, 1), cutpts = c(0, 3, 5, 9, 13, Inf))
GOF(data, nweiblik, pweib, start = c(1, 1), cutpts = c(0, 3, 5, 9, 13, Inf), 
    pearson = TRUE)

## ----hwe-gof-sol, warning=FALSE------------------------------------------
theta2probs <- function(theta) { 
    c(theta^2, 2*theta*(1-theta), (1-theta)^2)  
}
loglik <- function(theta, x) {
  probs <- theta2probs(theta)
  if (any(probs < 0)) return (NA)
	dmultinom(x, sum(x), theta2probs(theta), log = TRUE)
}

geno<-c(83, 447, 470)
ml <- maxLik(loglik, start = 0.5, x = geno); ml
theta.hat <- coef(ml); theta.hat

chisq.test(geno, p = theta2probs(theta.hat))
# so we can grab that statistic and redo the p-value:
X <- stat(chisq.test(geno, p = theta2probs(coef(ml)))); X
1 - pchisq(X, df = 2 - 1)  # df = 2 for multinomial, 1 for model based on theta

## ----hwe-gof-man---------------------------------------------------------
o <- geno
e <- theta2probs(theta.hat) * sum(o); e
testStats <- c(lrt = 2 * sum(o * log (o / e)), pearson= sum((o - e)^2 / e))
testStats
1-pchisq(testStats, df = 2-1)

## ----fisher-plants-sol, warning = FALSE----------------------------------
fisher.counts <- c(1997, 906, 904, 32)
# computes model probabilities from value of theta
theta2probs <- function(theta) {
    c(0.25 * (2 + theta), 
      0.25 * (1-theta), 
      0.25 * (1-theta), 
      0.25 * theta )
}

## ----fisher-plants-ll-sol, warning = FALSE-------------------------------
# direct calculation
loglik.fisher <- function(theta, x) {
  if (theta < 0 || theta > 1) return(NA)
  (  x[1]         * log(0.25 * (2 + theta)) 
  + (x[2] + x[3]) * log(0.25 * (1 - theta))
  +  x[4]         * log(0.25 * theta)
  )
}
# using dmultinom()
loglik.fisher2 <- function(theta, x) {
    if (theta < 0 || theta > 1) { return (NA) }
    dmultinom(x, size = sum(x), prob = theta2probs(theta), log = TRUE)
}


## ----fisher-plants-a-sol, warning = FALSE--------------------------------
ml.fisher  <- maxLik(loglik.fisher,  start = 0.5, x = fisher.counts); ml.fisher
ml.fisher2 <- maxLik(loglik.fisher2, start = 0.5, x = fisher.counts); ml.fisher2
theta.hat <- coef(ml.fisher)

## ----fisher-plants-bcd-sol,warning = FALSE-------------------------------
# test a specific value of theta vs. best possible theta
testTheta <- 
  Vectorize(
    vectorize.args = "theta0",
    function(theta0, x) {
      w <- 2 * (loglik.fisher(theta.hat, x) - loglik.fisher(theta0, x))
      p.value <- 1 - pchisq(w, df = 1)
      return(c(theta0 = theta0, w = w, p.value = p.value))
    }
  )
testTheta(c(0.03, 0.05, 0.07), x = fisher.counts) %>% t() %>% data.frame()

## ----fisher-plants-e-sol,warning = FALSE---------------------------------
o <- fisher.counts
e <- theta2probs(theta.hat) * sum(o)
testStats <- c(G = 2 * sum(o * log (o / e)), pearson = sum((o - e)^2 / e))
testStats
1-pchisq(testStats, df = 3-1)

## ----fisher-plants-chisq.test-sol----------------------------------------
chisq.test(fisher.counts, p = theta2probs(theta.hat))
# so we can grab that statistic and redo the p-value:
X <- stat(chisq.test(fisher.counts, p = theta2probs(theta.hat))); X
1 - pchisq(X, df = 2)

## ----fisher-plants-f-sol, tidy=FALSE-------------------------------------
fisher.pval <- 
  Vectorize(
    vectorize.args = "theta0",
    function(theta0, x) {
      w <- 2 * (loglik.fisher(theta.hat, x) - loglik.fisher(theta0, x))
      1 - pchisq(w, df = 1)
    }
  )
lo <- 
  uniroot(
    function(t0) fisher.pval(t0, fisher.counts) - 0.05, c(0, theta.hat)) %>% 
  value()
hi <- 
  uniroot(
    function(t0) fisher.pval(t0, fisher.counts) - 0.05, c(1, theta.hat)) %>% 
  value()
c(lo, hi)

## ----fisher-plants-wald-sol----------------------------------------------
SE <- stdEr(ml.fisher); SE
theta.hat + c(-1, 1) * qnorm(0.975) * SE

## ----mendel-pea-cross-sol------------------------------------------------
o <- c(315, 102, 108, 31)
n <- sum(o)
e <- n * c(9, 3, 3, 1) / 16; e
G <- 2 * sum(o * log(o / e)); G
pval <- 1-pchisq(G, 3); pval

## ----family-smoking01----------------------------------------------------
smokeTab <- tally(student ~ parents, data = FamilySmoking) 
smokeTab

## ----family-smoking-manual-----------------------------------------------
rowTotal <- rowSums(smokeTab); rowTotal
colTotal <- colSums(smokeTab); colTotal
grandTotal <- sum(smokeTab); grandTotal
e <- outer(rowTotal, colTotal) / grandTotal; e
o <- smokeTab
stat <- sum ((e - o)^2 / e); stat
pval <- 1 - pchisq(stat, df = 2); pval

## ----family-smoking02----------------------------------------------------
chisq.test(smokeTab)

## ----family-smoking-attr-------------------------------------------------
attributes((chisq.test(smokeTab)))

## ----family-smoking03----------------------------------------------------
xchisq.test(smokeTab)

## ----family-smoking-mosaic, eval=FALSE, message = FALSE------------------
## vcd::mosaic( ~ student + parents, data = FamilySmoking,
##              shade = TRUE)

## ----family-smoking-mosaic-fig, echo=FALSE-------------------------------
vcd::mosaic( ~ student + parents, data = FamilySmoking, 
             shade = TRUE)

## ----smoking-ads01, tidy = FALSE-----------------------------------------
smTab <- rbind(NonExperimenter = c(171, 15, 148), 
                  Experimenter = c(89, 10, 132))
colnames(smTab) = c("Never", "Hardly Ever", "Sometimes or a lot")
smTab
chisq.test(smTab)

## ----smoking-ads02-------------------------------------------------------
xchisq.test(smTab)

## ----smoking-bbs01, tidy = FALSE-----------------------------------------
smTab2 <- rbind(NonExperimenter = c(34, 4, 296), 
                   Experimenter = c(15, 3, 213))
colnames(smTab2) <- c("Never", "Hardly ever", "Sometimes or a lot")
smTab2
chisq.test(smTab2)

## ----smoking-sims--------------------------------------------------------
chisq.test(smTab2, simulate.p.value = TRUE, B = 5000)

## ----smoking-bbs02-------------------------------------------------------
smTab2[, -2]
chisq.test(smTab2[, -2])

## ----python--------------------------------------------------------------
python <- rbind(c(27-16, 16), c(56-38, 38), c(104-75, 75))
rownames(python) <- c("cold", "neutral", "hot")
colnames(python) <- c("unhatched", "hatched")
python
chisq.test(python)

## ----phs-----------------------------------------------------------------
phs <- cbind(c(104, 189), c(10933, 10845))
rownames(phs) <- c("aspirin", "placebo")
colnames(phs) <- c("heart attack", "no heart attack")
phs
xchisq.test(phs)

## ----chisq-twins, tidy=FALSE---------------------------------------------
convictions <- rbind(dizygotic   = c(2, 15), 
                     monozygotic = c(10, 3))
colnames(convictions) <- c("convicted", "not convicted")
convictions
chisq.test(convictions, correct = FALSE)
chisq.test(convictions) %>% pval()
fisher.test(convictions) %>% pval()

## ----amd-----------------------------------------------------------------
amd <- rbind(cases = c(27,  17,  6), controls = c(13,  46,  37))
dom <- rbind(cases = c(27 + 17,  6), controls = c(13 + 46,  37))
rec <- rbind(cases = c(27,  17 + 6), controls = c(13,  46 + 37))
chisq.test(amd)
chisq.test(dom)
chisq.test(rec)

## ----fusion1-merge-------------------------------------------------------
# merge fusion1 and pheno keeping only id's that are in both
Fusion1m <- merge(FUSION1, Pheno, by = "id", all = FALSE)

## ----fusion1-tally-geno--------------------------------------------------
tally( ~ t2d + genotype, data = Fusion1m)

## ----fusion1-tally-dose--------------------------------------------------
tally( ~ t2d + Gdose, data = Fusion1m)

## ----fusion1m-3models-sol------------------------------------------------
tally(t2d ~ genotype, data = Fusion1m) 
chisq.test(tally( ~ t2d + genotype, data = Fusion1m))
chisq.test(tally( ~ t2d + (Tdose >= 1), data = Fusion1m))
chisq.test(tally( ~ t2d + (Tdose <= 1), data = Fusion1m))

## ----nfl-prep, tidy = FALSE----------------------------------------------
NFL <- NFL2007 %>% mutate(
  dscore = homeScore - visitorScore,
  winner = ifelse(dscore > 0, home, visitor),
  loser  = ifelse(dscore > 0, visitor, home),
  homeTeamWon = dscore > 0
  )
head(NFL, 3)

## ----nfl-bt, tidy = FALSE, message = FALSE-------------------------------
# fit Bradley-Terry model
require(BradleyTerry2)
NFL.model <- 
  BTm(cbind(homeTeamWon, !homeTeamWon), home, visitor, data = NFL, id = "team") 

## ----nfl-post01, tidy=FALSE----------------------------------------------
bta <- BTabilities(NFL.model)
nflRatings<- data.frame(
    team = rownames(bta),
    rating = bta[, "ability"],
    se = bta[, "s.e."],
    wins = as.vector(tally( ~ winner, data = NFL)),
    losses = as.vector(tally( ~ loser, data = NFL))
    )
rownames(nflRatings) <- NULL

nflRatings[rev(order(nflRatings$rating)), ]

## ----nfl-post02----------------------------------------------------------
NFL <- NFL %>% 
  mutate(
    winnerRating = nflRatings$rating[as.numeric(winner)],
    loserRating  = nflRatings$rating[as.numeric(loser)], 
    upset = loserRating > winnerRating,
    pwinner = ilogit(winnerRating - loserRating))
# how big an upset was the Super Bowl?
NFL %>% tail(1)

## ----ncaa2010-bt-prep, tidy = FALSE--------------------------------------
NCAA <- NCAAbb %>% 
  filter(season == "2009-10", !postseason) %>%
  mutate(
    neutralSite = grepl("n", notes, ignore.case = TRUE), # at neutral site?
    homeTeamWon = hscore > ascore)                     # did home team win?
# remove teams that didn't play >= 5 at home and >=5 away
# (typically div II teams that played a few div I teams)
h <- tally( ~ home, data = NCAA); a <- tally( ~ away, data = NCAA)
deleteTeams <- c(names(h[h <= 5]), names(a[a <= 5]))
NCAA <- NCAA %>% 
  filter(!(home %in% deleteTeams | away %in% deleteTeams))

## ----ncaa2010-bt-fit, cache=TRUE, tidy = FALSE---------------------------
# fit a Bradley-Terry model
require(BradleyTerry2)
NCAA.model <- 
  BTm(cbind(homeTeamWon, 1 - homeTeamWon), 
       home, away, data = NCAA, refcat = "Kansas")

## ----ncaa2010-bt-look----------------------------------------------------
# look at top teams 
coef(NCAA.model)[rev(order(coef(NCAA.model)))[1:6]]

## ----ncaa2010-bt-hc-fit, cache = TRUE------------------------------------
require(BradleyTerry2)
# home team gets advantage unless on neutral court
NCAA$homeTeam <- data.frame(team = NCAA$home, at.home = 1 - NCAA$neutralSite)
NCAA$awayTeam <- data.frame(team = NCAA$away, at.home = 0)
NCAA.model2 <- 
  BTm(cbind(homeTeamWon, 1-homeTeamWon),
      homeTeam, awayTeam, id = "team", formula = ~ team + at.home, data = NCAA)

## ----ncaa2010-bt-hc-look-------------------------------------------------
# the "order effect" is the coefficient on "at.home"
coef(NCAA.model2)["at.home"] -> oe; oe
# expressed a multiplicative odds factor
exp(oe)
# prob home team wins if teams are "equal"
ilogit(oe)   

## ----ncaa2010-bt-hc-look2, tidy=FALSE------------------------------------
ab <- 
  BTabilities(NCAA.model2) 
ratings <- 
  ab[order(-ab[, "ability"]), ]
ratings[1:13, ]

## ----ncaa2010-bt-hc-look2b-----------------------------------------------
ratings[14:30, ]

## ----ncaa2010-compare, tidy = FALSE--------------------------------------
compareTeams <-  
  function(team1, team2, model, 
           abilities = BTabilities(model)) { 
    a <- abilities[team1, 1]
    b <- abilities[team2, 1]
    return(ilogit(a - b))
} 
compareTeams("Kansas", "Kentucky", ab = ratings)
compareTeams("Michigan St.", "Butler", ab = ratings)
compareTeams("Butler", "Duke", ab = ratings)

## ----binom-credible01----------------------------------------------------
qbeta(c(0.025, 0.975), 20 + 1, 30 + 1)
confint(binom.test(20, 50))             # for comparison
confint(prop.test(20, 50))              # for comparison

## ----binom-credible02----------------------------------------------------
qbeta(c(0.025, 0.975), 38 + 1, 62 + 1)
confint(binom.test(38, 100))            # for comparison
confint(prop.test(38, 100))             # for comparison

## ----binom-bayes-pval----------------------------------------------------
1- pbeta(0.5, 38 + 1, 62 + 1)          # 1-sided Bayesian p-value
pval(binom.test(38, 100, alt = "less"))      # for comparison
pval(prop.test(38, 100, alt = "less"))       # for comparison

## ----bayes-normal--------------------------------------------------------
x <- c(20, 24, 27, 28, 28, 28, 29, 30, 
	   30, 30, 30, 32, 33, 34, 35, 38)
mean(x)
sd(x)
posterior <- function(x, mu0, sigma0, sigma = 5) {
    n <- length(x)
    N <- (n * mean(x) / sigma^2 + mu0 / sigma0^2)
    D <- (n / sigma^2 + 1 / sigma0^2)
    mu1 <- N / D; sigma1 <- sqrt(1 / D)  
    precision1 <- D
    precision0 <- 1 / sigma0^2
    precision.data <- n / sigma^2
    return(cbind(mu1, sigma1, precision1, precision0, precision.data))
    }
posterior(x, 20, 1)
posterior(x, 20, 4)
posterior(x, 20, 16)
posterior(x, 20, 1000)
5 / sqrt(length(x))

## ----dispersion-sol01----------------------------------------------------
val <- c(0,1,2,3,4)
frequency<- c(9,2,3,0,1)
n <- sum(frequency); n
x.bar <- sum(val * frequency) / n; x.bar
v <- sum(frequency * (val - x.bar)^2) / (n - 1); v
T <- 14 * v / x.bar; T
1- pchisq(T, 14)

## ----dispersion-sol02, seed = 12345--------------------------------------
T <- function(x) c(T = var(x) / mean(x))
# one-sided p-values
Sims1 <- (do(10000) * T(rpois(15, lambda = 1))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims5 <- (do(10000) * T(rpois(15, lambda = 5))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims50 <- (do(10000) * T(rpois(15, lambda = 50))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
# It isn't necessary to mulitiply T by (n-1) to assess linearity in a qq-plot
xqqmath(~ T, data = Sims1, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 1")
xqqmath(~ T, data = Sims5, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 5")
xqqmath(~ T, data = Sims5, distribution = function(p) qchisq(p, df = 14), type = "l",
        main = "lambda = 50")
# now we compare p-values to uniform distribution, zooming in on small p-values
xqqmath( ~ p.val, data = Sims1, distribution = qunif, type = "l", main = "lambda = 1", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))
xqqmath( ~ p.val, data = Sims5, distribution = qunif, type = "l", main = "lambda = 5", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))
xqqmath( ~ p.val, data = Sims50, distribution = qunif, type = "l", main = "lambda = 50", 
         xlim = c(0, 0.1), ylim = c(0, 0.1))


## ----LinearModels, child="LinearModels.Rnw", eval=includeChapter[6]------

## ----LM-packages, include = FALSE, cache = FALSE-------------------------
require(fastR2)

## ----include = FALSE, cache = FALSE--------------------------------------
knitr::opts_chunk$set(cache.path = "cache/LM-")

## ----eval = FALSE--------------------------------------------------------
## response ~ predictor1 + predictor2 + predictor3

## ----eval = FALSE--------------------------------------------------------
## response ~ 1 + predictor1 + predictor2 + predictor3

## ----eval = FALSE--------------------------------------------------------
## response ~ -1 + predictor1 + predictor2 + predictor3

## ----eval = FALSE--------------------------------------------------------
## y ~ 1           # note: explicit intercept is the only term on right

## ----eval = FALSE--------------------------------------------------------
## length ~ weight        # using implicit intercept
## length ~ 1 + weight    # using explicit intercept

## ----eval = FALSE--------------------------------------------------------
## height ~ sex           # using implicit intercept
## height ~ 1 + sex       # using explicit intercept

## ----eval = FALSE--------------------------------------------------------
## cholesterol ~ age + I(age^2)

## ----eval = FALSE--------------------------------------------------------
## gpa ~ SATM + SATV
## gpa ~ I(SATM + SATV)

## ----small-data01--------------------------------------------------------
SmallData <- data.frame(x = c(1, 2, 3, 4), y = c(2, 5, 6, 8))
xyplot(y ~ x, data = SmallData)

## ----small-data02--------------------------------------------------------
model <- lm(y ~ x , data = SmallData) 
xyplot(y ~ x, data = SmallData, type = c("p", "r"))

## ----small-data03--------------------------------------------------------
coef(model)           # the coefficients
fitted(model)         # y-hat values
resid(model)          # residuals
SmallData$y - fitted(model)     # residuals again
msummary(model)        # a summary of the model

## ----small-data04--------------------------------------------------------
names(model)
names(msummary(model))
model$rank             # number of linearly independent cols in model matrix
summary(model)$sigma

## ----small-data05--------------------------------------------------------
x <- SmallData$x; y <- SmallData$y     
Sxx <- sum((x - mean(x))^2); Sxx
Sxy <- sum((x - mean(x)) * (y - mean(y))); Sxy
r <- 1 / 3 * sum((x - mean(x)) / sd(x) * (y - mean(y)) / sd(y)); r
slope <- r * sd(y) / sd(x); slope
intercept <- mean(y) - slope * mean(x); intercept

## ----small-data06--------------------------------------------------------
# set up the v an u vectors
v0 <- rep(1, 4); v0
u0 <- v0 / vlength(v0); u0
v1 <- x - mean(x); v1
u1 <- v1 / vlength(v1); u1
#
# projecting into the model space
project(y, v0)
project(y, v1)
project(y, v0) + project(y, v1)    # fitted values
fitted(model)
#
# two ways to compute beta_1-hat
b1 <- dot(y, v1) / (vlength(v1))^2; b1
b1 <- dot(y, u1) / (vlength(v1));   b1
#
# two ways to compute alpha_0-hat
a0 <- dot(y, v0) / (vlength(v0))^2; a0
a0 <- dot(y, u0) / (vlength(v0));   a0
#
# beta_0-hat
b0 <- a0 - b1 * mean(x); b0

## ----small-data07--------------------------------------------------------
# create model matrix
x <- SmallData$x; y <- SmallData$y
x
y
intercept <- rep(1, 4)
X <- cbind(intercept, x); X
# estimate coeficients
B <- solve(t(X) %*% X) %*% t(X)
B %*% y
# compute fitted values
H <- X %*% B
H %*% y

## ----small-data08--------------------------------------------------------
X <- model.matrix(model); X             

## ----trebuchet2, fig.show="hide"-----------------------------------------
treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
coef(treb.model)
xyplot(distance ~ projectileWt, data = Trebuchet2, type = c("p", "r"))

## ----trebuchet2-fig, echo=FALSE, results = "hide"------------------------
treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
coef(treb.model)
xyplot(distance ~ projectileWt, data = Trebuchet2, type = c("p", "r"))

## ----lm-elasticband, fig.show = "hide"-----------------------------------
data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband) 
coef(eband.model)
xyplot(distance ~ stretch, data = elasticband, type = c("p", "r"))

## ----lm-elasticband-fig, results = "hide", echo = FALSE------------------
data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband) 
coef(eband.model)
xyplot(distance ~ stretch, data = elasticband, type = c("p", "r"))

## ----Galton-regression01, tidy = FALSE-----------------------------------
GaltonBoys <- 
  Galton %>%
  filter(sex == "M") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup()
Galton.lm <- lm(height ~ father, data = GaltonBoys) 
coef(Galton.lm)
xyplot(height ~ father, data = GaltonBoys, type = c("p", "r"))

## ----Galton-regression02-------------------------------------------------
favstats( ~ height, data = GaltonBoys)
favstats( ~ father, data = GaltonBoys)

## ----Galton-regression03-------------------------------------------------
projectedHeight <- makeFun(Galton.lm) 
projectedHeight(father = 75)
projectedHeight(father = 65)

## ----Galton-regression04, tidy = FALSE-----------------------------------
GaltonBoys <-
  GaltonBoys %>%
  mutate(midparent = (father + mother) / 2)
favstats( ~ height, data = GaltonBoys)
favstats( ~ midparent, data = GaltonBoys)

## ----Galton-regression05, tidy = FALSE, fig.keep = "none"----------------
GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
xyplot(zheight ~ zmidparent, data = GaltonBoys, type = c("p", "r"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(0, 1, lty = 2, col = "gray70")
       }
)

## ----Galton-regression05-fig, echo = FALSE, results = "hide"-------------
GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
xyplot(zheight ~ zmidparent, data = GaltonBoys, type = c("p", "r"),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(0, 1, lty = 2, col = "gray70")
       }
)

## ----bands-sol-----------------------------------------------------------
model1 <- lm(distance ~ stretch, data = elasticband)
model2 <- lm(distance ~ stretch, data = RubberBand)
msummary(model1)
msummary(model2)

## ----trebuchet2-summary, tidy = FALSE------------------------------------
treb.model <- 
  lm(distance ~ projectileWt, data = Trebuchet2)
msummary(treb.model)   # terser output than summary() produces

## ----trebuchet2-ci-------------------------------------------------------
-0.0946 + c(-1, 1) * 0.01713 * qt(0.975, df = 14)  # CI by hand
confint(treb.model, "projectileWt")             # CI using confint()

## ----confint-lm----------------------------------------------------------
stats:::confint.lm

## ----lm-elasticband-ci---------------------------------------------------
msummary(eband.model)
4.554 + c(-1, 1) * 1.543 * qt(0.975, df = 5)          # CI by hand
confint(eband.model, "stretch")                   # CI using confint()

## ----anova-trebuchet2----------------------------------------------------
treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
anova(treb.model)

## ----trebuchet2-rsquared, tidy = FALSE-----------------------------------
rsquared(treb.model)

## ----eval = FALSE--------------------------------------------------------
## lm(y ~  0 + x)
## lm(y ~ -1 + x)

## ----anova-elasticband---------------------------------------------------
anova(eband.model)

## ----trebuchet2-predict, tidy = FALSE------------------------------------
treb.dist <- makeFun(treb.model)
treb.dist(projectileWt = 44)
treb.dist(projectileWt = 44, interval = "confidence")
treb.dist(projectileWt = 44, interval = "prediction")

## ----trebuchet2-plot2, fig.show = "hide", tidy = FALSE-------------------
xyplot(distance ~ projectileWt, 
    data = Trebuchet2, ylim = c(2.50, 10.50),
    panel = panel.lmbands, conf.lty = 1, pred.lty = 1
    )

## ----trebuchet2-plot2-fig, results = "hide", echo = FALSE, tidy = FALSE----
xyplot(distance ~ projectileWt, 
    data = Trebuchet2, ylim = c(2.50, 10.50),
    panel = panel.lmbands, conf.lty = 1, pred.lty = 1
    )

## ----elasticband-predict, tidy = FALSE-----------------------------------
eband.dist <- makeFun(eband.model)
eband.dist(stretch = 30)
eband.dist(stretch = 30, interval = "confidence")
eband.dist(stretch = 30, interval = "prediction")

## ----eband-fig, results = "hide", echo = FALSE---------------------------
xyplot(distance ~ stretch,
    data = elasticband, ylim = c(80, 210),
    panel = panel.lmbands,
    conf.lty = 1,
    pred.lty = 1
)

## ----resid-v-fit-fig, results = "hide", echo = FALSE---------------------
x <- seq(0, 10, by = 0.25)
resid <- rnorm(length(x))
y1 <- x + resid
y2 <- (x - 5)^2 / 5 + resid
y3 <- x + (0.75 + 0.25 * x) * resid
y4 <- x + resid
y4[35] <- x[35]- 2 * max(abs(resid))
resid1 <- resid(lm(y1 ~ x))
resid2 <- resid(lm(y2 ~ x))
resid3 <- resid(lm(y3 ~ x))
resid4 <- resid(lm(y4 ~ x))
stresid1 <- resid1 / sd(resid1)
stresid2 <- resid2 / sd(resid2)
stresid3 <- resid3 / sd(resid3)
stresid4 <- resid4 / sd(resid4)
fit1 <- fitted(lm(y1 ~ x))
fit2 <- fitted(lm(y2 ~ x))
fit3 <- fitted(lm(y3 ~ x))
fit4 <- fitted(lm(y4 ~ x))
group <- rep(toupper(letters[1:4]), each = length(x))
rdata <- data.frame(
            x = rep(x, times = 4),
            fit = c(fit1, fit2, fit3, fit4),
            residual = c(resid1, resid2, resid3, resid4),
            stresidual = c(stresid1, stresid2, stresid3, stresid4),
            group = group)
xyplot(stresidual ~ fit|group,
                    data = rdata,
                    ylab = "residual",
                    scales = list(x = list(relation = "free"), draw = F),
                    ylim = c(-1.1, 1.1) * max(abs(rdata$stresidual)),
                    as.table = T)

## ----lm-star01, tidy = FALSE, fig.keep = "none", message = FALSE---------
Stars <- faraway::star
star.plot1 <- xyplot(light ~ temp, data = Stars)
HotStars <- Stars %>% filter(temp > 3.7)   # select all but 4 coolest stars
star.model1 <- lm(light ~ temp, data = Stars)
star.model2 <- lm(light ~ temp, data = HotStars)
xyplot(light ~ temp, data = Stars,
    panel = function(x, y, ...){
        panel.abline(reg = star.model1, lwd = 2, lty = 1,
            col = trellis.par.get("superpose.line")$col[2])
        panel.abline(reg = star.model2, lwd = 2, lty = 1,
            col =  trellis.par.get("superpose.line")$col[1])
        panel.xyplot(x, y, ...)
        ids <- which(Stars$temp < 4.0)
        grid::grid.text(x = x[ids] + 0.04, y = y[ids],
            as.character(ids),
            default.units = "native", gp = grid::gpar(cex = 0.7))
    })

## ----lm-star-fig, results = "hide", echo = FALSE, message = FALSE--------
Stars <- faraway::star
star.plot1 <- xyplot(light ~ temp, data = Stars)
HotStars <- Stars %>% filter(temp > 3.7)   # select all but 4 coolest stars
star.model1 <- lm(light ~ temp, data = Stars)
star.model2 <- lm(light ~ temp, data = HotStars)
xyplot(light ~ temp, data = Stars,
    panel = function(x, y, ...){
        panel.abline(reg = star.model1, lwd = 2, lty = 1,
            col = trellis.par.get("superpose.line")$col[2])
        panel.abline(reg = star.model2, lwd = 2, lty = 1,
            col =  trellis.par.get("superpose.line")$col[1])
        panel.xyplot(x, y, ...)
        ids <- which(Stars$temp < 4.0)
        grid::grid.text(x = x[ids] + 0.04, y = y[ids],
            as.character(ids),
            default.units = "native", gp = grid::gpar(cex = 0.7))
    })

## ----lm-star-fig01, results = "hide", echo = FALSE-----------------------
plot(star.model1)

## ----lm-star-fig02, results = "hide", echo = FALSE-----------------------
plot(star.model2)

## ----plot-starmodels, fig.keep = "none"----------------------------------
plot(star.model1, w = 1:6) 
plot(star.model2, w = 1:6) 

## ----lm-star-dfbeta, fig.show = "hide", tidy = FALSE---------------------
xyplot(dfbeta(star.model2)[, "temp"] ~ index, 
    data = HotStars,
    ylab = "DFBETA",
    panel = function(x, y, ...) {
        ids <- which(abs(y) > 0.5)
        panel.xyplot(x, y, ...)
        grid::grid.text(
            x = x[ids] + 1.5, y = y[ids],
            as.character(ids), default.units = "native")
    })
coef(lm(light ~ temp, HotStars))
coef(lm(light ~ temp, HotStars[-7, ]))

## ----lm-star-dfbeta-fig, echo = FALSE, results = "hide"------------------
xyplot(dfbeta(star.model2)[, "temp"] ~ index, 
    data = HotStars,
    ylab = "DFBETA",
    panel = function(x, y, ...) {
        ids <- which(abs(y) > 0.5)
        panel.xyplot(x, y, ...)
        grid::grid.text(
            x = x[ids] + 1.5, y = y[ids],
            as.character(ids), default.units = "native")
    })
coef(lm(light ~ temp, HotStars))
coef(lm(light ~ temp, HotStars[-7, ]))

## ----tukey-bulge01, results = "hide", echo = FALSE-----------------------
n <- 20  
x <- runif(n, 2, 10)
y <- exp(0.3 * x)
e <- exp(rnorm(n, 0, 0.1))
y <- y * e
foo <- function(x) {
    a <- x[2]
    x <- x[1]
    if (a == 0) { return (log(x)) }
    return (x^a)
}

power <- function(x, a) {
    M <- cbind(x, a)
    return  (apply(M, 1, foo))
}
powers <- c(0, 0.5, 1, 2,3); np <- length(powers)
a <- rep(rep(powers, each = n), each = np)
b <- rep(rep(powers, each = n), times = np)
x <- rep(x, times = n * np)
y <- rep(y, times = n * np)
X <- power(x, a)
Y <- power(y, b)
original <- (a==1 & b==1)
ddd <- data.frame(X = X, Y = Y, a = a, b = b, original = original)
xyplot(y ~ x)

## ----tukey-buldge-many-fig, results = "hide", echo = FALSE, opts.label = "figbig"----
latticeExtra::useOuterStrips(
xyplot(Y ~ X | paste("a=", a, sep = "") + paste("b=", b, sep = ""),
            ddd, groups = original,
            scales = list(relation = "free", draw = FALSE))
)

## ----balldrop, fig.show = "hide"-----------------------------------------
ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
xyplot(time ~ height, data = BallDrop, type = c("p", "r"))
plot(ball.model, w = 1)

## ----balldrop-fig, results = "hide", echo = FALSE------------------------
ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
xyplot(time ~ height, data = BallDrop, type = c("p", "r"))
plot(ball.model, w = 1)

## ----balldrop-trans, fig.show = "hide", cache = FALSE--------------------
ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
xyplot(time ~ height, data = BallDrop, panel = panel.lm, model = ball.modelT)
plot(ball.modelT, w = 1)

## ----balldrop-trans-fig, results = "hide", echo = FALSE, cache = FALSE----
ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
xyplot(time ~ height, data = BallDrop, panel = panel.lm, model = ball.modelT)
plot(ball.modelT, w = 1)

## ----balldrop-sol--------------------------------------------------------
lm(time^2 ~ height, data = BallDrop)
lm(log(time) ~ log(height), BallDrop)

## ----balldrop-avg, fig.show = "hide", tidy = FALSE, cache = FALSE--------
BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
msummary(ball.modelA)
xyplot(time ~ height, BallDropAvg,
                panel = panel.lm, model = ball.modelA)
plot(ball.modelA, w = 1)

## ----balldrop-avg-fig, results = "hide", echo = FALSE--------------------
BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
msummary(ball.modelA)
xyplot(time ~ height, BallDropAvg,
                panel = panel.lm, model = ball.modelA)
plot(ball.modelA, w = 1)

## ----lm-soap01-----------------------------------------------------------
Soap.model1 <- lm(weight ~ day, data = Soap)
msummary(Soap.model1)

## ----lm-soap02-----------------------------------------------------------
Soap.model2 <- lm(I(weight^(1/3)) ~ day, data = Soap)
msummary(Soap.model2)

## ----lm-soap-fig, results = "hide", echo = FALSE-------------------------
daysToFit <- seq(1, 22, by = 0.5)
linfits <- predict(Soap.model1, newdata = data.frame(day = daysToFit))
transfits <- predict(Soap.model2, newdata = data.frame(day = daysToFit))^3
xyplot(weight ~ day, data = Soap, 
    panel = function(x, y, ...) {
        panel.xyplot(daysToFit, linfits, lwd = 2, type = "l", 
            col = trellis.par.get("superpose.line")$col[1])
        panel.xyplot(daysToFit, transfits, lwd = 2, type = "l",
            col = trellis.par.get("superpose.line")$col[2])
        panel.xyplot(x, y, cex = 1.0, ...)
    }
)

## ----lm-soap03-----------------------------------------------------------
confint(Soap.model1) 

## ----prob-soap-----------------------------------------------------------
Soap2 <- Soap %>% filter(day < 20)
Soap.model2 <- lm(weight ~ day, data = Soap2)
msummary(Soap.model2)
plot(Soap.model2, w = 1:2)

## ----pendulum-sol01, warning = FALSE-------------------------------------
model <- lm(period ~ sqrt(length), data = Pendulum)
msummary(model)
confint(model)
f <- makeFun(model) 
plot(model, w = 1)
plot(model, w = 2)

## ----pendulum-sol02, fig.keep = "last", warning = FALSE------------------
xyplot(period ~ length, data = Pendulum)
plotFun(f(l) ~ l, add = TRUE)

## ----pendulum-sol03, warning = FALSE-------------------------------------
model2 <- lm(log(period) ~ log(length), data = Pendulum)
msummary(model2)
g <- makeFun(model2) 
plot(model2, w = 1)
plot(model2, w = 2)

## ----pendulum-sol04, fig.keep = "last", warning = FALSE------------------
xyplot(period ~ length, data = Pendulum)
plotFun(f(l) ~ l, add = TRUE)
plotFun(exp(g(l)) ~ l, add = TRUE, col = "red")

## ----pendulum-sol05------------------------------------------------------
confint(model2)

## ----cornnit-sol01-------------------------------------------------------
data(cornnit, package = "faraway")
xyplot(yield ~ log(1 + nitrogen), data = cornnit, type = c("p", "r"))
cornnit.mod <- lm(yield ~ log(1 + nitrogen), data = cornnit)
msummary(cornnit.mod)
plot(cornnit.mod, w = 1:3)

## ----cornnit-sol02-------------------------------------------------------
xyplot(yield ~ log(1 + nitrogen), data = cornnit[-21, ], type = c("p", "r"))
cornnit.mod2 <- lm(yield ~ log(1 + nitrogen), data = cornnit[-21, ])
msummary(cornnit.mod2)
plot(cornnit.mod2, w = 1:3)

## ----cornnit-sol03-------------------------------------------------------
xyplot(yield^2 ~ sqrt(nitrogen), data = cornnit, type = c("p", "r"))
cornnit.mod3 <- lm(yield^2 ~ sqrt(nitrogen), data = cornnit)
msummary(cornnit.mod3)
plot(cornnit.mod3, w = 1:3)

## ----eval = FALSE--------------------------------------------------------
## model1 <- lm(y1 ~ x1, data = anscombe)
## msummary(model1)

## ----eband-effects-------------------------------------------------------
data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband)
ef <- eband.model$effects; n <- length(ef)
ef
# total length
sum(ef^2)
sum(elasticband$distance^2)
# projection of residuals into n-2 orthogonal components
sum(ef[3:n]^2)
sum(resid(eband.model)^2)
# projection in direction of u[0] is mean * sqrt(n)
mean(elasticband$distance) * sqrt(n)
ef[1]
# beta1.hat obtained from projection in direction of u[1]
# Note: R's u[1] points in the opposite direction.
ef[2] / sqrt(sum((elasticband$stretch - mean(elasticband$stretch))^2))
coef(eband.model)

## ----trebuchet2-effects--------------------------------------------------
treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
ef <- treb.model$effects; n <- length(ef)
ef
# total length
sum(ef^2)
sum(Trebuchet2$distance^2)
# projection of residuals into n-2 orthogonal components
sum(ef[3:n]^2)
sum(resid(treb.model)^2)
# projection in direction of u[0] is mean * sqrt(n)
# Note: R's u[0] points in the opposite direction.
mean(Trebuchet2$distance) * sqrt(n)
ef[1]
# beta1.hat obtained from projection in direction of u[1]
v1 <- Trebuchet2$projectileWt - mean(Trebuchet2$projectileWt)
ef[2] / sqrt(sum(v1^2))
coef(treb.model)

## ----anova-from-lm00, include = FALSE, seed = 123------------------------
x = rep(1:5, each = 4)
y = 3 + 1 * x + rnorm(20, 0, 3)
someData = data.frame(x = x, y = y)

## ------------------------------------------------------------------------
msummary(lm(y ~ x, someData))

## ----anova-from-lm-sol01-------------------------------------------------


## ----anova-from-lm-sol02-------------------------------------------------
anova(lm(y ~ x, someData))

## ----act-gpa-sol---------------------------------------------------------
grades <- ACTgpa
t.test(grades$ACT)
t.test(grades$GPA)
grades.model <- lm(GPA ~ ACT, data = grades)
msummary(grades.model)
grades.plot1 <- xyplot(GPA ~ ACT, data = grades, panel = panel.lmbands)
act2gpa <- makeFun(grades.model)
act2gpa(ACT = 25, interval = "confidence")
act2gpa(ACT = 25, interval = "prediction")

## ----drag-sol------------------------------------------------------------
model1 <- lm(velocity^2 ~ force.drag, data = Drag)
model2 <- lm(velocity ~ sqrt(force.drag), data = Drag)
model3 <- lm(log(velocity) ~ log(force.drag), data = Drag)
msummary(model1)
msummary(model2)
msummary(model3)

## ----drag-fig, results = "hide", echo = FALSE----------------------------
xyplot(velocity^2 ~ force.drag, data= Drag, groups = height)
plot(model1, w = 1)
xyplot(velocity ~ force.drag, data = Drag, 
       scales = list(log = T), groups = height)
plot(model3, w = 1)

## ----spheres-sol01-------------------------------------------------------
xyplot(log(mass) ~ log(diameter), data = Spheres)
spheres.lm <- lm(log(mass) ~ log(diameter), data = Spheres)
confint(spheres.lm)
plot(spheres.lm, w = 1:2)

## ----spheres-sol02, fig.keep = "last"------------------------------------
mass <- makeFun(spheres.lm) 
xyplot(mass ~ diameter, data = Spheres)
plotFun(mass(x) ~ x, add = TRUE, lwd = 2, under = TRUE)

## ----spheres-sol03-------------------------------------------------------
confint(spheres.lm, level = .96)

## ----lm-taste01----------------------------------------------------------
favstats(score ~ scr, data = TasteTest)

## ----lm-taste02----------------------------------------------------------
taste.model <- lm(score ~ scr, data = TasteTest) 
msummary(taste.model)

## ----lm-taste03----------------------------------------------------------
confint(taste.model)

## ----lm-taste04----------------------------------------------------------
confint(taste.model, "scrfine") / 50  

## ----lm-corn, tidy = FALSE-----------------------------------------------
# the Corn data frame has an inconvenient "shape" 
# (each type of Corn is in its own column)
head(Corn, 3)                                   
# this puts all the yields in one column and type of seed in another
Corn2 <- stack(Corn)                         
Corn2[c(1, 2, 12, 13), ]
# the default variable names aren't great, so we rename them
names(Corn2) <- c("yield", "treatment")       
Corn2[c(1, 2, 12, 13), ]
favstats(yield ~ treatment, data = Corn2)
Corn.model <- lm(yield ~ treatment, data = Corn2)
msummary(Corn.model)

## ----paired-corn-sol-----------------------------------------------------
t.test(~ (reg-kiln), data = Corn)    # paired
t.test(Corn$reg, Corn$kiln)        # 2-sample

## ----tirewear-sol01------------------------------------------------------
msummary(lm(weight ~ groove, data = TireWear))
xyplot(weight ~ groove, data = TireWear, type = c("p", "r"))

## ----tirewear-sol02------------------------------------------------------
msummary(lm(weight ~ -1 + groove, data = TireWear))

## ----tirewear-sol03------------------------------------------------------
t.test(TireWear$weight, TireWear$groove, paired = TRUE)
t.test( ~ (weight - groove), data = TireWear)

## ----tirewear-sol04------------------------------------------------------
binom.test(~ (weight > groove), data = TireWear)
prop.test(~ (weight > groove), data = TireWear)

## ----oats-variety, tidy = FALSE------------------------------------------
oats <- data.frame(
  soil = 1:7,
  A = c(71.2, 72.6, 47.8, 76.9, 42.5, 49.6, 62.8), 
  B = c(65.2, 60.7, 42.8, 73.0, 41.7, 56.6, 57.3)
)
t.test( ~ (A - B), data = oats)

## ----t-corn--------------------------------------------------------------
t.test(Corn$kiln, Corn$reg)                 # 2-vector interface
t.test(yield ~ treatment, data = Corn2)     # formula interface

## ----taste-t-------------------------------------------------------------
t.test(score ~ scr, data = TasteTest)

## ----power-t-test01------------------------------------------------------
power.t.test(delta = 5, sd = 10, power = 0.8)

## ----power-t-test02------------------------------------------------------
power.t.test(delta = 0.5, power = 0.8)

## ----power-t-test03------------------------------------------------------
power.t.test(delta = 0.5, n = 50)

## ----power-t-test04------------------------------------------------------
power.t.test(delta = 0.25, n = 50)

## ----power-t-test05, fig.show = "hide"-----------------------------------
pow <- function(effect) {
    power.t.test(delta = effect, n = 50)$power
}
effect = seq(0, 2, by = 0.05)
xyplot(pow(effect) ~ effect, type= "l",
    ylab = "power", xlab = "effect size",
    main = "Power of a 2-sample test (n = 50)")

## ----power-t-test05-fig, results = "hide", echo = FALSE------------------
pow <- function(effect) {
    power.t.test(delta = effect, n = 50)$power
}
effect = seq(0, 2, by = 0.05)
xyplot(pow(effect) ~ effect, type= "l",
    ylab = "power", xlab = "effect size",
    main = "Power of a 2-sample test (n = 50)")

## ----power-t-test06------------------------------------------------------
power.t.test(delta = 0.5, power = 0.8, type = "one.sample")

## ----power-t-test07------------------------------------------------------
power.t.test(delta = 0.5, power = 0.8, type = "paired")

## ----orings01, tidy = FALSE----------------------------------------------
# select the version of this data set in the faraway package
data(orings, package = "faraway")        
orings <-
  orings %>% mutate(failure = damage != 0)   # convert to binary response
orings.model <- 
    glm(failure ~ temp, data = orings, family = binomial(link = logit))
msummary(orings.model)

## ----orings-fig, results = "hide", echo = FALSE--------------------------
temps <- seq(30, 100, by = 2)
xyplot(damage / 6 ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(-0.05, 1.05),
    ylab = "percent of O-rings damaged",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )
xyplot(failure ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(-0.05, 1.05),
    ylab = "probability of failure",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )

## ----orings-predict, tidy = FALSE, digits = 5----------------------------
# by default, predict() works on the linear model scale
r <- predict(orings.model, newdata = data.frame(temp = 31)); r
ilogit(r)
# but we can ask for it to work on the "response" scale
predict(orings.model, newdata = data.frame(temp = 31), type = "response")

## ----orings-makeFun, digits = 5------------------------------------------
# by default, makeFun() uses type = "response" and 
# returns values on data scale
temp2damage <- makeFun(orings.model) 
temp2damage(temp = 31) 
makeFun(orings.model)(31)   # We can do it all in one line if we prefer
# the other option is type = "link"
temp2damage <- makeFun(orings.model, type = "link")
temp2damage(temp = 31)

## ----orings-predict2, digits = 5-----------------------------------------
p <- makeFun(orings.model)(31)
1 - (1-p)^(1/6) -> q; q       # P(damage to particular O-ring)
1 - dbinom(0, 6, q)           # P(damage to >0 O-rings)
cbind(0:6, dbinom(0:6, 6, q)) # table of all probabilities

## ----orings-2, digits = 5------------------------------------------------
orings.model2 <-                  # link = logit is default, so unnecessary
    glm(cbind(damage, 6 - damage) ~ temp, data = orings, 
        family = binomial(link = logit))
msummary(orings.model2)
p1 <- predict(orings.model, newdata = data.frame(temp = 31), type = "response"); p1
p2 <- predict(orings.model2, newdata = data.frame(temp = 31), type = "response"); p2
dbinom(0, 6, prob = p2)               # 0 damaged O-rings
xyplot(damage / 6 ~ temp, data = orings, 
    xlim = c(30, 100),
    ylim = c(0, 1),
    ylab = "percent of O-rings damaged",
    alpha = 0.7,
    panel = function(x, y, ...){
        panel.xyplot(temps, 
            predict(orings.model, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2, col = "gray50", lty = 2)
        panel.xyplot(temps, 
            predict(orings.model2, type = "response", 
                newdata = data.frame(temp = temps)),
            type = "l", lwd = 2)
        panel.xyplot(x, y, ...)
    }
    )

## ----runswins-look-------------------------------------------------------
head(MLB2004, 4)

## ----runswins01, tidy = FALSE--------------------------------------------
BB <- MLB2004 %>% 
  mutate(runmargin = (R - OR) / G)

# data frame has summarized data for each team, so different syntax here:
glm.bb <- glm(cbind(W, L) ~ runmargin, data = BB, family = "binomial") 
msummary(glm.bb)

## ----runswins02, fig.keep = "none", tidy = FALSE-------------------------
BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(glm.bb)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

xyplot(winP ~ predWinP, data = BB,
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(0, 1)
    })

## ----runswins03, fig.keep = "none"---------------------------------------
rm <- seq(-5, 5, by = 0.1)
wp <- makeFun(glm.bb)(runmargin = rm)
xyplot(winP ~ runmargin, data = BB, xlim = c(-2.5, 2.5), ylim = c(0, 1),
    panel = function(x, y, ...){
        panel.xyplot(x, y, ...)
        panel.xyplot(rm, wp, type = "l", col = "gray50")
    })

## ----runswins02-fig, results = "hide", echo = FALSE----------------------
BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(glm.bb)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

xyplot(winP ~ predWinP, data = BB,
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(0, 1)
    })
rm <- seq(-5, 5, by = 0.1)
wp <- makeFun(glm.bb)(runmargin = rm)
xyplot(winP ~ runmargin, data = BB, xlim = c(-2.5, 2.5), ylim = c(0, 1),
    panel = function(x, y, ...){
        panel.xyplot(x, y, ...)
        panel.xyplot(rm, wp, type = "l", col = "gray50")
    })

## ----orings-ci01---------------------------------------------------------
s <- summary(orings.model)
sqrt(diag(s$cov.unscaled)) -> st.err; st.err
coef(orings.model)[2] + c(-1, 1) * st.err[2] * qnorm(0.975)
exp(coef(orings.model)[2] + c(-1, 1) * st.err[2] * qnorm(0.975))

## ----orings-ci02---------------------------------------------------------
confint(orings.model, parm = "temp")

## ----runswins-sol01, tidy = FALSE----------------------------------------
BB <- 
  MLB2004 %>% 
  mutate(
    runmargin = (R - OR) / G,
    winP = W / G)
glm.bb <-
  glm(cbind(W, L) ~ runmargin, data = BB, family = binomial()) 
lm.bb <- lm(winP ~ runmargin, data = BB) 
msummary(lm.bb)
BB <- BB %>% 
  mutate(
    glmPredWinP = makeFun(glm.bb)(runmargin = runmargin), 
    lmPredWinP = makeFun(lm.bb)(runmargin = runmargin)
    )
plot(lm.bb, w = 2)
plot(glm.bb, w = 2)
# observations 8 and 27 have largest residuals
BB[c(8, 27, 1:2, 29:30), c("team", "winP", "glmPredWinP", "lmPredWinP")]

## ----buckthorn-sol01, fig.keep = "last"----------------------------------
buck.model <- 
  glm(dead ~ conc, data = Buckthorn, family = binomial)
msummary(buck.model)
dead <- makeFun(buck.model) 

## ----buckthorn-sol02-----------------------------------------------------
odds <- exp(coef(buck.model)[1]); odds     # odds when conc = 0
# prob when conc = 0
odds / (1 + odds)
ilogit(coef(buck.model)[1])  
dead(0)

## ----buckthorn-sol03-----------------------------------------------------
odds <- function(p) { p / (1 - p) }
odds(dead(0.01)) / odds(dead(0))
odds(dead(0.30)) / odds(dead(0.29))

## ----buckthorn-sol04, fig.keep = "last", tidy = FALSE--------------------
# calculate the percentage dead at each concentration used.
tbl <- 
  Buckthorn %>%
  group_by(conc) %>%
  summarise( 
    total = n(),
    obsDead = sum(dead),
    obsAlive = sum(!dead),
    propDead = obsDead / (obsDead + obsAlive)
  ) %>%
  mutate( 
    expDead = dead(conc) * total,
    expAlive = (1 - dead(conc)) * total,
    expPropDead = dead(conc)
  )
tbl
xyplot(propDead ~ conc, data = tbl,
    ylab = "predicted death rate", xlab = "concentration of glyphosate",
)
plotFun(dead(c) ~ c, add = TRUE)

## ----include = FALSE, eval = FALSE---------------------------------------
## tbl2 <- Buckthorn %>%
##   group_by(conc) %>%
##   summarise(
##     propDead = mean(dead)
##     )
## tbl2
## concentrations = seq(0, 0.5, by = 0.02)
## fits <- predict(buck.model, new = data.frame(conc = concentrations),
##             type = "response")

## ----buckthorn-sol05-----------------------------------------------------
observed <- tbl[, 3:4]; observed 
expected <- tbl[, 6:7]; expected
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson
# pvals
1 - pchisq(pearson, df = 2)
1 - pchisq(lrt, df = 2)

## ----logit-probit-sol, fig.keep = "last"---------------------------------
plotFun(ilogit(3 + 2 * x) ~ x, x.lim = c(-6, 3), lwd = 3, col = "gray70")
plotFun(pnorm(1.5 * b1 + b1 * x) ~ x, b1 = sqrt(2 * pi)/2, 
        add = TRUE, col = "red")

## ----runmargin-sol, fig.keep = "last", tidy = FALSE----------------------
bb.logit <-
  glm(cbind(W, L) ~ runmargin, data = BB, 
      family = binomial(link = logit)) 
bb.probit <-
  glm(cbind(W, L) ~ runmargin, data = BB, 
      family = binomial(link = probit))
confint(bb.logit)
confint(bb.probit)
f.logit <- makeFun(bb.logit) 
f.probit <- makeFun(bb.probit)
plotFun(f.logit(r) ~ r, r.lim = c(-2, 2))
plotFun(f.probit(r) ~ r, add = TRUE, 
        lty = 2, lwd = 5, alpha = .4, col = "black")

## ----orings-sol, fig.keep = "last", tidy = FALSE-------------------------
orings.logit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = logit))
orings.probit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = probit))
confint(orings.logit)
confint(orings.probit)
g.logit <- makeFun(orings.logit) 
g.probit <- makeFun(orings.probit) 
plotFun(g.logit(t) ~ t, t.lim = c(25, 90))
plotFun(g.probit(t) ~ t, add = TRUE, 
        lty = 2, lwd = 5, alpha = .4, col = "black")

## ----buckthorn-probit-sol01, tidy = FALSE--------------------------------
buck.model2 <- 
  glm(dead ~ conc, data = Buckthorn, family = binomial(link = probit))
msummary(buck.model2)

## ----buckthorn-probit-sol02, fig.keep = "last", tidy = FALSE-------------
dead2 <- makeFun(buck.model2) 
tbl2 <- 
  Buckthorn %>% 
  group_by(conc) %>%
  summarise( 
    total = length(dead),
    obsDead = sum(dead),
    obsAlive = sum(!dead),
    propDead = sum(dead) / length(dead)
  ) %>%
  mutate(
    expDead = dead2(conc) * total,
    expAlive = (1 - dead2(conc)) * total,
    expPropDead = dead2(conc)
  )
tbl2
xyplot(propDead ~ conc, data = tbl2,
    ylab = "predicted death rate", xlab = "concentration of glyphosate",
)
plotFun(dead2(c) ~ c, add = TRUE)
plotFun(dead(c) ~ c, add = TRUE, col = "gray60", lwd = 3, lty = 2)

## ----bucktorn-probit-sol03, tidy = FALSE---------------------------------
observed <- tbl2[ , 3:4]
expected <- tbl2[ , 6:7]
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson

## ------------------------------------------------------------------------
# pvals
1 - pchisq(pearson, df = 2)
1 - pchisq(lrt, df = 2)

## ----lm-sim01------------------------------------------------------------
b0 <- 3; b1 <- 5; sigma <- 2       # set model parameters
x <- rep(1:5, each = 4)            # 4 observations at each of 5 values
e <- rnorm(length(x), sd = sigma)  # error term in the model
y <- b0 + b1 * x + e               # build response according to model
model <- lm(y ~ x); msummary(model)
confint(model)

## ----lm-sim02, cache = TRUE, tidy = FALSE--------------------------------
sim <- 
  function(
    b0 = 3, b1 = 5, sigma = 2, 
    x = rep(1:5, each = 4)         # 4 observations at each of 5 values
    ){
    e <-rnorm(length(x), sd = sigma)
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }
Sims <- do(5000) * sim()
Sims <-
  Sims %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 
tally( ~ status, data = Sims)

binom.test( ~ status, data = Sims, p = 0.95)

## ----lm-sim03------------------------------------------------------------
chisq.test(tally( ~ status, data = Sims), p = c(0.95, 0.025, 0.025))

## ----lm-sim04, cache = TRUE, tidy = FALSE--------------------------------
sim2 <- 
  function(
    b0 = 3, b1 = 5, lambda = 1,
    x = rep(1:5, each = 4)       # 4 observations at each of 5 values
    ){
    # shift to give a mean of 0.
    e <- rexp(length(x), rate = 1 / lambda) - lambda
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }
Sims2 <- do(5000) * sim2()
Sims2 <-
  Sims2 %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 
tally( ~ status, data = Sims2) / 5000

binom.test( ~ status, data = Sims2, p = 0.95)
chisq.test(tally( ~ status, data = Sims2), p = c(0.95, 0.025, 0.025))

## ----lm-sim05, cache = TRUE, tidy = FALSE--------------------------------
sim3 <- 
  function(
    b0 = 3, b1 = 5, lambda = 1, 
    x = rep(1:5, each = 4)    # 4 observations at each of 5 values
    ){
    e <- x * rnorm(length(x))
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }
Sims3 <- do(5000) * sim3()
Sims3 <-
  Sims3 %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 
tally( ~ status, data = Sims3) / 5000

binom.test( ~ status, data = Sims3, p = 0.95)
chisq.test(tally( ~ status, data = Sims3), p = c(0.95, 0.025, 0.025))

## ----qr, tidy = FALSE----------------------------------------------------
QRdata <- data.frame(x = c(1, 1, 5, 5), y = c(1, 2, 4, 6))
qr.model <- lm(y ~ x, data = QRdata)
Q <- qr.model %>% qr() %>% qr.Q(); Q
R <- qr.model %>% qr() %>% qr.R(); R

## ----backsolve-----------------------------------------------------------
backsolve(R, t(Q) %*% QRdata$y)
coef(qr.model)

## ----glm-guassian-sol, tidy = FALSE--------------------------------------
glm(stretch ~ distance, data = elasticband, 
    family = gaussian()) %>% 
  msummary()
lm(stretch ~ distance, data = elasticband) %>% msummary()
lm(stretch ~ distance, data = elasticband) %>% anova()


## ----RegressionVariations, child="RegressionVariations.Rnw", eval=includeChapter[7]----

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Reg-")
require(multcomp)
require(effects)

## ----punting01, tidy = FALSE---------------------------------------------
punting.lm <- 
  lm(distance ~ rStrength + rFlexibility, data = Punting) 
summary(punting.lm)
anova(punting.lm)

## ----punting02, fig.show = "hide", tidy = FALSE--------------------------
lm(rFlexibility ~ rStrength, data = Punting) %>% msummary()
xyplot(rStrength ~ rFlexibility, data = Punting)
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r 
r^2

## ----punting02-fig, echo = FALSE, results = "hide", cache = FALSE--------
lm(rFlexibility ~ rStrength, data = Punting) %>% msummary()
xyplot(rStrength ~ rFlexibility, data = Punting)
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r 
r^2

## ----punting03, eval = FALSE---------------------------------------------
## plot(punting.lm, w = 1:2)

## ----punting03-fig, echo = FALSE, results = "hide"-----------------------
plot(punting.lm, w = 1:2)

## ----punting07, tidy = FALSE---------------------------------------------
puntingFit <- makeFun(punting.lm)
puntingFit(rStrength = 175, rFlexibility = 100, interval = "confidence")
puntingFit(rStrength = 175, rFlexibility = 100, interval = "prediction")

## ----concrete01----------------------------------------------------------
concrete.lm1 <- lm(strength ~ limestone + water, data = Concrete)
msummary(concrete.lm1)

## ----concrete03----------------------------------------------------------
y <- Concrete$strength
n <- length(y); v0 <- rep(1, n)
v1 <- with(Concrete, limestone - mean(limestone))
v2 <- with(Concrete, water - mean(water))
dot(y, v0) / vlength(v0)^2
mean(y)
dot(y, v1) / vlength(v1)^2
dot(y, v2) / vlength(v2)^2

## ----concrete04----------------------------------------------------------
y <- Concrete$strength
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
fitted(concrete.lm1)

## ----concrete05----------------------------------------------------------
dot(v1, v2)

## ----concrete-minus01----------------------------------------------------
# modify data by dropping first observation
Concretemod <- Concrete[-1, ]
concrete.lmmod <- lm(strength ~ limestone + water, data = Concretemod)
coef(concrete.lmmod)
y <- Concretemod$strength
n <- length(y); v0 <- rep(1, n)
v1 <- with(Concretemod, limestone - mean(limestone))
v2 <- with(Concretemod, water - mean(water))
project(y, v0)
mean(y)
dot(y, v1) / vlength(v1)^2
dot(y, v2) / vlength(v2)^2
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
fitted(concrete.lmmod)

## ----concrete-minus02----------------------------------------------------
dot(v0, v1)
dot(v0, v2)
dot(v1, v2)

## ----concrete-minus03----------------------------------------------------
w1 <- v1 - project(v1, v2)
w2 <- v2 - project(v2, v1)
dot(v0, w1)
dot(v0, w2)
dot(v1, w2)
dot(w1, v2)

## ----concrete-minus04----------------------------------------------------
y <- Concretemod$strength
# make fits using v1 and w2
ef0 <- project(y, v0)
ef1 <- project(y, v1)
ef2 <- project(y, w2)
ef0 + ef1 + ef2
# now try w1 and v2
ef0 <- project(y, v0)
ef1 <- project(y, w1)
ef2 <- project(y, v2)
ef0 + ef1 + ef2
# should match what lm() produces
fitted(concrete.lmmod)

## ----concrete-minus05----------------------------------------------------
# using v1 gives coefficient in model with 
# only limestone as a predictor
dot(y, v1) / vlength(v1)^2
lm(strength ~ limestone, data = Concretemod) %>% coef()
# using v2 gives coefficient in model with only water as a predictor
dot(y, v2) / vlength(v2)^2
lm(strength ~ water, data = Concretemod) %>% coef()
# using w1 and w2 gives coefficients in the model 
dot(y, w1) / vlength(w1)^2
dot(y, w2) / vlength(w2)^2
coef(concrete.lmmod)

## ----concrete-Q01--------------------------------------------------------
Q <- 
  cbind( 
    w1 / vlength(w1)^2, 
    w2 / vlength(w2)^2)
t(Q) %*% y

## ----concrete-Q02--------------------------------------------------------
x1 <- Concretemod$limestone; x2 <- Concretemod$water
Q <- 
  cbind( 
    1 / nrow(Concretemod),
    w1 / vlength(w1)^2, 
    w2 / vlength(w2)^2)
alpha <- t(Q) %*% y; alpha
beta0 <- alpha[1] - alpha[2] * mean(x1) - alpha[3] * mean(x2)
beta0

## ----concrete-QR01, tidy = FALSE-----------------------------------------
X <- cbind(1, x1, x2); X
Q <- cbind(
  1/sqrt(nrow(Concretemod)), 
  v1 / vlength(v1), 
  w2 / vlength(w2)) 
Q %>% round(4)
t(Q) %*% Q %>% round(4)           # should be the identity matrix
R <- t(Q) %*% X; R %>% round(4)   # should be upper triangular
Q %*% R %>% round(4)              # should be X

## ----concrete-QR02-------------------------------------------------------
solve(R) %*% t(Q) %*% y
backsolve(R, t(Q) %*% y)

## ----concrete-QR03-------------------------------------------------------
diag(R)
c(vlength(v0), vlength(v1), vlength(v2))

## ----concrte-QR04--------------------------------------------------------
t(Q) %*% y  / c(vlength(v0), vlength(v1), vlength(w2))

## ----concrete-QR05-------------------------------------------------------
concrete.lmmod %>% qr() %>% qr.Q()
concrete.lmmod %>% qr() %>% qr.R()

## ----small-lmfit-sol01---------------------------------------------------
y <- c(0, 2, 0, 2, -1, 6)
x1 <- c(1, 1, 2, 2, 3, 3); x2 <- c(0, 1, 1, 2, 1, 3)
v0 <- rep(1, length(y))
v1 <- x1 - mean(x1); v2 = x2 - mean(x2)
w1 <- v1 - project(v1, v2)
w2 <- v2 - project(v2, v1)

## ----small-lmfit-sol02---------------------------------------------------
#
# obtaining model fits by projection
#
p0 <- project(y, v0); p0
p1 <- project(y, v1); p1
p2 <- project(y, v2); p2
q1 <- project(y, w1); q1
q2 <- project(y, w2); q2
#
# this won't be a correct fit because dot(v1, v2) != 0
#
p0 + p1 + p2  

## ----small-lmfit-sol03---------------------------------------------------
#
# here is the correct fit 
#
p0 + q1 + p2
p0 + p1 + q2

## ----small-lmfit-sol04---------------------------------------------------
#
# we can compare the results with those from lm()
#
model <- lm(y ~ x1 + x2); fitted(model)        
#
# this won't work to get the coefficients:
#
b1.wrong <- (p1/v1); b1.wrong   
b2.wrong <- (p2/v2); b2.wrong   

## ----small-lmfit-sol05---------------------------------------------------
#
# now let's get the coefficients correctly:
#
b1 <- (q1/w1); b1   
b2 <- (q2/w2); b2  
a0 <- (p0/v0); a0
b0 <- a0 - b1*mean(x1) - b2*mean(x2); b0
coef(model)

## ----small-QR-sol--------------------------------------------------------
X <- cbind(1, x1, x2)
Q <- cbind(
  v0 / vlength(v0),
  v1 / vlength(v1),
  w2 / vlength(w2)
)

# orthogonality check for Q
t(Q) %*% Q %>% round(3)

# solve QR = X for R and check that it is upper diagonal
R <- t(Q) %*% X; R %>% round(3)  

# check that X = QR (up to round off)
range(X - Q %*% R)          

# find coefficients
solve(R) %*% t(Q) %*% y

# check that this matches coefficients from lm()
range( solve(R) %*% t(Q) %*% y - coef(model) )

## ----concrete-mods01-----------------------------------------------------
# define several models
concrete.lm0 <- lm(strength ~ limestone + water, data = Concrete)
concrete.lm1 <- lm(strength ~ -1 + limestone + water, data = Concrete)
concrete.lm2 <- lm(strength ~ water, data = Concrete)
concrete.lm3 <- lm(strength ~ limestone, data = Concrete)
concrete.lm4 <- lm(strength ~ 1, data = Concrete)
concrete.lm5 <- lm(strength ~ I(limestone + water), data = Concrete)

## ----concrete-mods02-----------------------------------------------------
msummary(concrete.lm0)

## ----concrete-mods03-----------------------------------------------------
anova(concrete.lm1, concrete.lm0) # with/without intercept
anova(concrete.lm2, concrete.lm0) # with/without limestone
anova(concrete.lm3, concrete.lm0) # with/without water
anova(concrete.lm4, concrete.lm0) # with/without limestone and water

## ----concrete-mods04-----------------------------------------------------
confint(concrete.lm0)

## ----concrete-rand01, warning = FALSE------------------------------------
concrete.lm1 <- lm(strength ~ limestone, data = Concrete) 
anova(concrete.lm1)
concrete.r7 <- lm(strength ~ limestone + rand(7), data = Concrete)
anova(concrete.r7)

## ----include = FALSE-----------------------------------------------------
RSS1 <- sum(resid(concrete.lm1)^2)
RSS0 <- sum(resid(concrete.lm0)^2)

## ----concrete-rand02-----------------------------------------------------
anova(concrete.lm0)

## ----concrete-rand03, fig.keep = "none", message = FALSE, warning = FALSE, seed = 123----
SSplot(
  lm(strength ~ limestone + water, data = Concrete),
  lm(strength ~ limestone + rand(7), data = Concrete), n = 1000) 

## ----concrete-rand03-fig, echo = FALSE, results = "hide", message = FALSE, warning = FALSE, seed = 123----
SSplot(
  lm(strength ~ limestone + water, data = Concrete),
  lm(strength ~ limestone + rand(7), data = Concrete), n = 1000) 
last_plot() + xlim(0, 2)

## ----concrete-rand04, fig.keep = "none", message = FALSE, warning = FALSE, seed = 123----
SSplot(
  lm(strength ~ water + limestone, data = Concrete),
  lm(strength ~ water + rand(7), data = Concrete), n = 100) 

## ----concrete-rand04-fig, echo = FALSE, results = "hide", message = FALSE, warning = FALSE, seed = 123----
SSplot(
  lm(strength ~ water + limestone, data = Concrete),
  lm(strength ~ water + rand(7), data = Concrete), n = 100) 
last_plot() + xlim(0, 2)

## ----concrete-aic--------------------------------------------------------
# these two methods give different numerical values
AIC(concrete.lm0)
AIC(concrete.lm1)
extractAIC(concrete.lm0)
extractAIC(concrete.lm1)
# but differences between models are equivalent
AIC(concrete.lm0) - AIC(concrete.lm1)
extractAIC(concrete.lm0)[2] - extractAIC(concrete.lm1)[2]

## ----concrete-plot, fig.show = "hide"------------------------------------
plot(concrete.lm0, which = c(1, 2, 3, 5))

## ----concrete-plot-fig, echo = FALSE, results = "hide"-------------------
plot(concrete.lm0, which = c(1, 2, 3, 5))

## ----punting04-----------------------------------------------------------
# regressions of y and x1 on x2
punting.lmy2 <- lm(distance ~ rFlexibility, data = Punting)
punting.lm12 <- lm(rStrength ~ rFlexibility, data = Punting)
# regressions of y and x2 on x1
punting.lmy1 <- lm(distance ~ rStrength, data = Punting)
punting.lm21 <- lm(rFlexibility ~ rStrength, data = Punting)
# these slopes match coefficients from y ~ x1 + x2
coef(lm(resid(punting.lmy2) ~ resid(punting.lm12)))
coef(lm(resid(punting.lmy1) ~ resid(punting.lm21)))
coef(punting.lm)

## ----punting05, fig.keep = "none"----------------------------------------
# partial regression plots (a.k.a. added-variable plots)
xyplot(resid(punting.lmy2) ~ resid(punting.lm12), type = c("p", "r"))
xyplot(resid(punting.lmy1) ~ resid(punting.lm21), type = c("p", "r"))

## ----punting05-fig, echo = FALSE, results = "hide"-----------------------
# partial regression plots (a.k.a. added-variable plots)
xyplot(resid(punting.lmy2) ~ resid(punting.lm12), type = c("p", "r"))
xyplot(resid(punting.lmy1) ~ resid(punting.lm21), type = c("p", "r"))

## ----punting06, opts.label = "fig1", fig.keep = "none"-------------------
car::avPlots(punting.lm)

## ----punting06-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
car::avPlots(punting.lm)

## ----concrete-effects01, fig.keep = "none"-------------------------------
require(effects)
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("water")
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("limestone")

## ----concrete-effects01-fig, echo = FALSE, results = "hide"--------------
require(effects)
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("water")
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("limestone")

## ----concrete-effects02, fig.keep = "none"-------------------------------
Effect(
  c("water", "limestone"), concrete.lm0,  partial.resid = TRUE) %>% 
  plot("water")

## ----concrete-effects02-fig, echo = FALSE, results = "hide"--------------
Effect(
  c("water", "limestone"), concrete.lm0,  partial.resid = TRUE) %>% 
  plot("water")

## ----concrete-effect04---------------------------------------------------
concrete.lm6 <- 
  lm(strength ~ limestone + water + limestone:water, data = Concrete)

## ----concrete-effect05, fig.keep = "none"--------------------------------
lm(strength ~ limestone + water + limestone:water, 
   data = Concrete) %>%
  Effect(c("water", "limestone"), . , partial.residuals = TRUE) %>%
  plot("water")

## ----concrete-effect05-fig, echo = FALSE, results = "hide"---------------
lm(strength ~ limestone + water + limestone:water, 
   data = Concrete) %>%
  Effect(c("water", "limestone"), . , partial.residuals = TRUE) %>%
  plot("water")

## ----concrete-effect06---------------------------------------------------
lm(strength ~ limestone + water, data = Concrete) %>% 
  msummary()
lm(strength ~ limestone + water + limestone * water, data = Concrete) %>% 
  msummary()

## ----effect-sim, fig.keep = "none", seed = 1234--------------------------
D <- data_frame(
  x1 = runif(100, 0, 10),
  x2 = runif(100, 0, 10),
  y1 = 5 + 2 * x1 + 3 * x2 + rnorm(100, sd = 4),
  y2 = 5 + 2 * x1 + 3 * x2 - x1 * x2 + rnorm(100, sd = 4)
)
lm(y1 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y2 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y1 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y2 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")

## ----effect-sim-fig, echo = FALSE, results = "hide", seed = 1234---------
D <- data_frame(
  x1 = runif(100, 0, 10),
  x2 = runif(100, 0, 10),
  y1 = 5 + 2 * x1 + 3 * x2 + rnorm(100, sd = 4),
  y2 = 5 + 2 * x1 + 3 * x2 - x1 * x2 + rnorm(100, sd = 4)
)
lm(y1 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y2 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y1 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")
lm(y2 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1")

## ----utilities-kwh01-----------------------------------------------------
Utilities2 <- 
  Utilities %>% 
  filter(year > 2000 | month > 6) %>%  # remove bad meter reading
  filter(temp <= 60) %>%               # remove warm months 
  mutate(kwhpday = kwh / billingDays)

## ----utilities-kwh02, fig.show = "hide"----------------------------------
# fit additive and interaction models
ut.lm    <- lm(thermsPerDay ~ temp + kwhpday, data = Utilities2)
ut.lmint <- lm(thermsPerDay ~ temp * kwhpday, data = Utilities2)
msummary(ut.lm)
msummary(ut.lmint)
plot(ut.lm, 1:2)
plot(ut.lmint, 1:2)
ut.lm %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "additive model")
ut.lmint %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "interation model")

## ----utilities-kwh02-fig, echo = FALSE, results = "hide"-----------------
# fit additive and interaction models
ut.lm    <- lm(thermsPerDay ~ temp + kwhpday, data = Utilities2)
ut.lmint <- lm(thermsPerDay ~ temp * kwhpday, data = Utilities2)
msummary(ut.lm)
msummary(ut.lmint)
plot(ut.lm, 1:2)
plot(ut.lmint, 1:2)
ut.lm %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "additive model")
ut.lmint %>% 
  Effect(c("temp", "kwhpday"), . , partial.residuals = TRUE) %>% 
  plot("temp", sub = "interation model")

## ----utilities-kwh03-----------------------------------------------------
coef(ut.lmint)[1] +  coef(ut.lmint)[3] * 25
coef(ut.lmint)[2] +  coef(ut.lmint)[4] * 25
coef(ut.lm)

## ----utilities-month01, fig.show = "hide"--------------------------------
# remove first few observations because of bad meter read
Ut3 <- Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Ut3)
msummary(ut.lm3)
plotModel(ut.lm3)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month")
plot(ut.lm3, w = 1:2)

## ----utilities-month-fig, echo = FALSE, results = "hide"-----------------
# remove first few observations because of bad meter read
Ut3 <- Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Ut3)
msummary(ut.lm3)
plotModel(ut.lm3)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month")
plot(ut.lm3, w = 1:2)

## ----utilities-month02, fig.keep = "none"--------------------------------
Ut3 <- Ut3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Ut3)
msummary(ut.lm4)
plotModel(ut.lm4)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

## ----utilities-month02-fig, echo = FALSE, results = "hide"---------------
Ut3 <- Ut3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Ut3)
msummary(ut.lm4)
plotModel(ut.lm4)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

## ----utilities-month03---------------------------------------------------
ut.lm4a <- lm(thermsPerDay ~ poly(monthShifted, 2), data = Ut3)
msummary(ut.lm4a)
favstats( ~ (fitted(ut.lm4a) - fitted(ut.lm4)))

## ----utilities-month-sol-------------------------------------------------
ut.mod <-
  Ut3 %>% mutate(monthShifted2 = (month + 5) %% 12) %>%
  lm(thermsPerDay ~ poly(monthShifted2, 2), data = .) 
msummary(ut.mod)
plot(ut.mod, 1:2)
ut.mod %>% Effect("monthShifted2", ., partial.residuals = TRUE) %>%
  plot("monthShifted2")

## ----eval = FALSE--------------------------------------------------------
## px <- poly(Ut2$monthShifted, 2); px

## ------------------------------------------------------------------------
    model1 <- lm (thermsPerDay ~ monthShifted, data = Ut2)
    model2 <- lm (thermsPerDay ~ monthShifted + I(monthShifted^2), data = Ut2)
model1poly <- lm (thermsPerDay ~ poly(monthShifted, 1),  data = Ut2)
model2poly <- lm (thermsPerDay ~ poly(monthShifted, 2),  data = Ut2)

## ----gpa01---------------------------------------------------------------
gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
msummary(gpa.lm)

## ----gpa02---------------------------------------------------------------
gpa.lm1<- lm(gpa ~ satm, data = GPA)
msummary(gpa.lm1)

## ----gpa03---------------------------------------------------------------
gpa.lm2 <- lm(satm ~ satv + act, data = GPA); msummary(gpa.lm2)
gpa.lm3 <- lm(satm ~ satv, data = GPA); msummary(gpa.lm3)
gpa.lm4 <- lm(satm ~ act,  data = GPA); msummary(gpa.lm4)

## ----gpa04---------------------------------------------------------------
gpa.lm5 <- lm(gpa ~ act + satv, data = GPA); msummary(gpa.lm5)
gpa.lm6 <- lm(satv ~ act, data = GPA); msummary(gpa.lm6)

## ----gpa-mct01-----------------------------------------------------------
# fit some models
#
gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
gpa.lma <- lm(gpa ~ -1 + satm + satv + act, data = GPA)
#
# model comparison tests for 5 p-values in msummary(gpa.lm)
#
anova(gpa.lma, gpa.lm)

## ----gpa-mct-sol01-------------------------------------------------------
# fit some models
gpa.lm <- lm(gpa ~ satm + satv + act, data = GPA)
gpa.lma <- lm(gpa ~ -1 + satm + satv + act, data = GPA)
gpa.lmb <- lm(gpa ~ satv + act, data = GPA)
gpa.lmc <- lm(gpa ~ satm + act, data = GPA)
gpa.lmd <- lm(gpa ~ satm + satv, data = GPA)
gpa.lme <- lm(gpa ~ 1, data = GPA)

## ----gpa-mct-sol02-------------------------------------------------------
# model comparison tests for 5 p-values in msummary(gpa.lm)
anova(gpa.lma, gpa.lm)
anova(gpa.lmb, gpa.lm)

## ----gpa-mct-sol03-------------------------------------------------------
anova(gpa.lmc, gpa.lm)
anova(gpa.lmd, gpa.lm)

## ----gpa-mct-sol04-------------------------------------------------------
anova(gpa.lme, gpa.lm)
msummary(gpa.lm)

## ----gpa-mct-sol05-------------------------------------------------------
# combined SAT verses subscore
gpa.lmf <- lm(gpa ~ I(satv + satm) + act, data = GPA)
anova(gpa.lmf, gpa.lm)

## ----pheno-weight-sol----------------------------------------------------
# testing beta_1 = 2
t <- ( 1.0754 - 2.0) / 0.0121; t
2 * pt(-abs(t) , df = 2237)
# testing beta_1 = 1
t <- ( 1.0754- 1.0) / 0.0121; t
2 * pt(-abs(t) , df = 2237)
# testing beta_2 = 1
t <- ( 0.8942 - 1.0) / 0.0302; t
2 * pt(-abs(t) , df = 2237)

## ----students-sol01------------------------------------------------------
summary(Students)

## ----students-sol02------------------------------------------------------
summary(Students)
model <- lm(ACT ~ SAT, data = Students); msummary(model)

## ----students-sol03------------------------------------------------------
confint(model)
confint(lm( act ~ I(satm + satv), data = GPA))

## ----pheno-weight01------------------------------------------------------
pheno.lm <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno)
msummary(pheno.lm)

## ----pheno-weight02------------------------------------------------------
confint(pheno.lm) 

## ----pheno-weight03, fig.show = "hide"-----------------------------------
plot(pheno.lm, w = 1:2)

## ----pheno-weight-fig, echo = FALSE, results = "hide"--------------------

## ----pheno-weight03------------------------------------------------------
plot(pheno.lm, w = 1:2)

## ----pheno-sex-sol-------------------------------------------------------
pheno.male <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno %>% filter(sex == "M"))
pheno.female <- 
  lm(log(weight) ~ log(waist) + log(height), data = Pheno %>% filter(sex == "F"))
msummary(pheno.male)
msummary(pheno.female)
plot(pheno.male)     # males only
plot(pheno.female)   # females only
plot(pheno.lm)       # all subjects

## ----pheno-case-sol01, opts.label = "figbig"-----------------------------
pheno.case <- lm(log(weight) ~ log(waist) + log(height),
                 data = Pheno %>% filter(t2d == "case"))
pheno.control<- lm(log(weight) ~ log(waist) + log(height),
                 data = Pheno %>% filter(t2d == "control"))
msummary(pheno.case)
msummary(pheno.control)

## ----pheno-case-sol02, opts.label = "figbig"-----------------------------
plot(pheno.case)    # cases only
plot(pheno.control) # controls only
plot(pheno.lm)      # all subjects

## ----pheno-plain-sol-----------------------------------------------------
pheno.plain <- lm(weight ~ waist + height, data = Pheno)
c(plain = rsquared(pheno.plain), transformed = rsquared(pheno.lm))
c(plain = AIC(pheno.plain), transformed = AIC(pheno.lm))
plot(pheno.plain, w = 2)
plot(pheno.lm, w = 2)

## ----coag01, fig.show = "hide"-------------------------------------------
data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
xyplot(coag ~ diet, coagulation)
bwplot(coag ~ diet, coagulation)

## ----coag01-fig, echo = FALSE, results = "hide"--------------------------
data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
xyplot(coag ~ diet, coagulation)
bwplot(coag ~ diet, coagulation)

## ----coag02--------------------------------------------------------------
coag.lm <- lm(coag ~ diet, data = coagulation)
msummary(coag.lm)

## ----coag03, tidy = FALSE------------------------------------------------
coag.lm  <- lm(coag ~ diet, data = coagulation)
coag.lm1 <- lm(coag ~ 1,    data = coagulation)
anova(coag.lm1, coag.lm)

## ----coag04--------------------------------------------------------------
anova(coag.lm)

## ----coag05--------------------------------------------------------------
data(coagulation, package = "faraway")
# group-by-group tally
coagulation %>%
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>%
  summarise(n = n(), group.mean = mean(coag), 
            SSE = sum((coag - group.mean)^2),
            SSM = sum((group.mean - grand.mean)^2),
            SST = sum((coag - grand.mean)^2))

# individuala tally
coagulation <- 
  coagulation %>% 
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>% mutate(group.mean = mean(coag)) %>%
  ungroup()
coagulation %>% sample(5)

data.frame(
  SST = sum( ~ (coag - grand.mean)^2, data = coagulation),
  SSE = sum( ~ (coag - group.mean)^2, data = coagulation),
  SSM = sum( ~ (group.mean - grand.mean)^2, data = coagulation))

## ----coag04r-------------------------------------------------------------
anova(coag.lm)

## ----coag02r-------------------------------------------------------------
coag.lm <- lm(coag ~ diet, data = coagulation)
msummary(coag.lm)

## ----coag06, eval = FALSE------------------------------------------------
## model.matrix(coag.lm)

## ----coag-alt01----------------------------------------------------------
coag.altmodel <- lm(coag ~ -1 + diet, data = coagulation)
msummary(coag.altmodel)

## ----coag-alt02, eval = FALSE--------------------------------------------
## model.matrix(coag.altmodel);

## ----coag-recenter-------------------------------------------------------
data(coagulation, package = "faraway")
Coag0 <-
  coagulation %>%
  mutate(grand.mean = mean(coag)) %>%
  group_by(diet) %>% 
  mutate(coag0 = coag - grand.mean) %>%
  ungroup()
lm(coag0 ~ -1 + diet, data = Coag0) %>%
  msummary()

## ----airp01--------------------------------------------------------------
mean(pollution ~ location, data = AirPollution)

## ----airp02--------------------------------------------------------------
airp.lm <- lm(pollution ~ location, data = AirPollution)
anova(airp.lm)

## ----seed = 1234, include = FALSE, digits = 2----------------------------
Study <- data.frame(
  type = rep(LETTERS[1:3], each = 5),
  yield = rnorm(15, mean = rep(c(17.5, 19, 20), each = 5), sd = 0.75)
)

## ----digits = 3----------------------------------------------------------
favstats(yield ~ type, data = Study)

## ----anova-table-sol-----------------------------------------------------
favstats(yield ~ type, data = Study)
anova(lm(yield ~ type, data = Study))
group.means <- round(c(mean(yield ~ type, data = Study)), 1); group.means
y.bar <- 5 * sum(group.means) / 15; y.bar
group.sds <- round(sd(yield ~ type, data = Study), 3); group.sds
data_frame(
  SSE = sum(4 * group.sds^2), MSE = SSE / 12, 
  SSM = sum(5 * (group.means - y.bar)^2), MSM = SSM / 2, 
  F = MSM/MSE, p = 1 - pf(F, 2, 12))

## ----petstress-sol01-----------------------------------------------------
pet.lm <- lm(rate ~ group, data = PetStress)
favstats(rate ~ group, data = PetStress)

## ----petstress-sol02-----------------------------------------------------
anova(pet.lm)

## ----petstress-sol03-----------------------------------------------------
msummary(pet.lm)

## ----airp-modcomp01, tidy = FALSE----------------------------------------
# convert location to a numeric variable for convenience
AirP <- AirPollution %>%
  mutate(loc = as.numeric(location))
model <- lm(pollution ~ location, data = AirP)
model2 <- lm(pollution ~  1 + (loc == 3), data = AirP)
anova(model2, model)

## ----airp-modcomp02------------------------------------------------------
# build a variable that makes the model easier to describe 
AirP <- AirP %>% mutate(x  = (loc == 2) + 0.5 * (loc == 3))
model3 <- lm(pollution ~ 1 + x, data = AirP)
anova(model3, model)

## ----airp-modcomp03, tidy = FALSE----------------------------------------
# build two variables that make the model easier to describe 
AirP <- AirP %>% mutate(
  x1 = (loc == 1) + 0.5 * (loc == 3),
  x2 = (loc == 2) + 0.5 * (loc == 3))
model3 <- lm(pollution ~ -1 + x1 + x2, data = AirP)
anova(model3, model)

## ----airp-vectors--------------------------------------------------------
u1 <- 1/2 * c(1, 1, -1, -1, 0, 0)
u2 <- 1 / sqrt(12) * c(1, 1, 1, 1, -2, -2)
dot(AirPollution$pollution, u1)
dot(AirPollution$pollution, u2)
t1 <- dot(AirPollution$pollution, u1) / sqrt(202/3); t1
t2 <- dot(AirPollution$pollution, u2) / sqrt(202/3); t2
t1^2
t2^2
2 * pt( - abs(t1), df = 3)
2 * pt( - abs(t2), df = 3)

## ----coag07--------------------------------------------------------------
msummary(coag.lm)

## ----airp-TukeyHSD-------------------------------------------------------
airp.lm <- lm(pollution ~ location, data = AirPollution)
TukeyHSD(airp.lm)

## ----airp-glht01, fig.keep = "none"--------------------------------------
require(multcomp) 
airp.cint <- confint(glht(airp.lm, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.lm))  

## ----airp-glht01-fig, echo = FALSE, results = "hide"---------------------
require(multcomp) 
airp.cint <- confint(glht(airp.lm, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.lm))  

## ----coag-TukeyHSD-------------------------------------------------------
coag.lm <- lm(coag ~ diet, data = coagulation)
TukeyHSD(coag.lm)

## ----coag-glht, fig.show="hide"------------------------------------------
require(multcomp)
coag.glht <- glht(coag.lm, mcp(diet = "Tukey"))
msummary(coag.glht)  
plot(confint(coag.glht))
mplot(TukeyHSD(coag.lm)) 

## ----coag-glht-fig, echo = FALSE, results = "hide", message = FALSE------
require(multcomp)
coag.glht <- glht(coag.lm, mcp(diet = "Tukey"))
msummary(coag.glht)  
plot(confint(coag.glht))
mplot(TukeyHSD(coag.lm)) 

## ----airp-glht02, tidy = FALSE-------------------------------------------
airp.lm1 <- lm(pollution ~ location, data = AirPollution)
# specify contrasts by giving the coefficients
contr <- rbind(
	c(0, 1, 0),
	c(0, 0.5, -1))
# we can give our contrasts custom names if we like
contr1 <- rbind(
	"hill - plains" = c(0, 1, 0),
	"suburb - urban" = c(0, 0.5, -1))
msummary(glht(airp.lm1, contr1))

## ----airp-glht03, tidy = FALSE-------------------------------------------
# these look nicer if we parameterize differently in the model
airp.lm2 <- lm(pollution ~ -1 + location, data = AirPollution)
contr2 <- rbind(
	"hill - plains" = c(1, -1, 0),
	"suburb - urban" = c(1, 1, -2))
msummary(glht(airp.lm2, contr2))

## ----airp-glht04, tidy = FALSE-------------------------------------------
# using mcp() to help build the contrasts
airp.lm3 <- lm(pollution ~ location, data = AirPollution)
contr3 <- mcp(location = rbind(
	"hill - plains" = c(1, -1, 0),
	"suburb - urban" = c(1, 1, -2)
	))
msummary(glht(airp.lm3, contr3))

## ----airp-glht05---------------------------------------------------------
# unadjusted p-values 
2 * pt(-0.731, df = 3)
2 * pt(-2.533, df = 3)

## ----airp-glht06, tidy = FALSE-------------------------------------------
airp.lm4 <- lm(pollution ~ location, data = AirPollution)
contr4 <- mcp(location = rbind(
	"hill - plains" = c(1, -1, 0)))
msummary(glht(airp.lm4, contr4))

## ----cholesterol01, fig.show = "hide"------------------------------------
data(cholesterol, package = "multcomp")
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

## ----cholesterol01-fig, echo = FALSE, results = "hide"-------------------
data(cholesterol, package = "multcomp")
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

## ----cholesterol02, fig.keep = "none"------------------------------------
chol.glht <- confint(glht(chol.lm, mcp(trt = "Tukey")))
msummary(chol.glht)
plot(confint(chol.glht))

## ----cholesterol02-fig, echo = FALSE, results = "hide"-------------------
chol.glht <- confint(glht(chol.lm, mcp(trt = "Tukey")))
msummary(chol.glht)
plot(confint(chol.glht))

## ----cholesterol03, tidy = FALSE-----------------------------------------
glht(chol.lm, 
     mcp(trt = 
           rbind(
             "1time - 2times" = c(1, -1, 0, 0, 0),
             "(1 or 2 times) - 4times" = c(0.5, 0.5, -1, 0, 0),
             "new - old" = c(2, 2, 2, -3, -3)/6,
             "drugD - drugE" = c(0, 0, 0, 1, -1))
     )) %>%
  summary()

## ----cholesterol04, tidy = FALSE-----------------------------------------
confint(glht(chol.lm, mcp(trt = 
    rbind(
        "new - old" = c(2, 2, 2, -3, -3)/6)
    )))

## ----coag-dunnet---------------------------------------------------------
glht(coag.lm, mcp(diet = "Dunnet")) %>% 
  summary()

## ----taste-anova, fig.show = "hide"--------------------------------------
favstats(score ~ type, data = TasteTest)
xyplot(score ~ type, data = TasteTest)
taste.lm <- lm(score ~ type, data = TasteTest)
anova(taste.lm)
taste.cint <- confint(glht(taste.lm, mcp(type = "Tukey"))); taste.cint
plot(taste.cint)

## ----taste-anova-fig, echo = FALSE, results = "hide"---------------------
favstats(score ~ type, data = TasteTest)
xyplot(score ~ type, data = TasteTest)
taste.lm <- lm(score ~ type, data = TasteTest)
anova(taste.lm)
taste.cint <- confint(glht(taste.lm, mcp(type = "Tukey"))); taste.cint
plot(taste.cint)

## ----cholesterol05-------------------------------------------------------
chol.lm1 <- lm(response ~ trt, data = cholesterol)
cholesterol <- 
  cholesterol %>% 
  mutate(x1 = trt == "drugD", x2 = trt == "drugE")
chol.lm2 <- lm(response~ 1 + x1 + x2 , cholesterol)
anova(chol.lm1, chol.lm2)

## ----bugs----------------------------------------------------------------
model <- aov(sqrt(trapped) ~ color, data = Bugs)
TukeyHSD(model)
model <- lm(sqrt(trapped) ~ color, data = Bugs)
glht(model, mcp(color = "Tukey")) %>%
  summary()          

## ----taste01-------------------------------------------------------------
mean(score ~ scr + liq, data = TasteTest, .format = "table")

## ----taste02-------------------------------------------------------------
taste.lm <- lm(score ~ scr * liq, data = TasteTest)
anova(taste.lm)

## ----taste03-------------------------------------------------------------
taste.lm <- lm(score ~ scr * liq, data = TasteTest)
msummary(taste.lm)

## ----taste04-------------------------------------------------------------
M <- cbind(                                # model matrix
        "C1" = rep(c(-1, -1, 1, 1), each = 4)/8,     # C1
        "C2" = rep(c(-1, 1, -1, 1), each = 4)/8,     # C2
        "C3" = rep(c(1, -1, -1, 1), each = 4)/4      # C3
        )
taste.lm2 <- lm(score ~ M, data = TasteTest)
msummary(taste.lm2)

## ----taste05-------------------------------------------------------------
NTaste <- data.frame(score = TasteTest$score,
                     scr   = as.numeric(TasteTest$scr) - 1,
                     liq   = as.numeric(TasteTest$liq) - 1,
                     scrliq = ( as.numeric(TasteTest$scr) -1 ) * 
                              ( as.numeric(TasteTest$liq) -1 )
                     );  NTaste

Omega <- lm(score ~ scr * liq, data= TasteTest)
M <- model.matrix(Omega)
M2 <- cbind(M[, 3], M[, 2] - 2 * M[, 4])
M3 <- cbind(M[, 2], M[, 3] - 2 * M[, 4])

omega1  <- lm(score ~ scr + liq, data = TasteTest)
omega2  <- lm(score ~ M2, data = TasteTest)
omega2a <- lm(score ~ liq + I(scr - 2 * scrliq), data = NTaste)
omega3  <- lm(score ~ M3, data = TasteTest)
omega3a <- lm(score~ scr + I(liq - 2 * scrliq), data = NTaste)

anova(omega1, Omega)   # test for interaction
# test main effect for scr
# anova(omega2a, Omega)  # this gives the same result as line below
anova(omega2, Omega)   
# test main effect for liq
# anova(omega3a, Omega)  # this gives the same result as line below
anova(omega3, Omega)   

## ----noise01-------------------------------------------------------------
noise.lm <- lm(score ~ noise + group, data = MathNoise)
anova(noise.lm)
favstats(score ~ group, data = MathNoise)

## ----noise02-------------------------------------------------------------
noise.lm2 <- lm(score ~ noise * group, data = MathNoise)
anova(noise.lm2)

## ----noise03, fig.show = "hide"------------------------------------------
xyplot(score ~ noise, groups = group, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))
xyplot(score ~ group, groups = noise, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))

## ----noise03-fig, echo = FALSE, results = "hide", seed = 1234------------
xyplot(score ~ noise, groups = group, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))
xyplot(score ~ group, groups = noise, data = MathNoise,
    type = c("p", "a"), jitter.x = TRUE,
    auto.key = list(lines = TRUE, points = TRUE, columns = 2))

## ----poison01------------------------------------------------------------
poison.lm <- 
  lm(time ~ factor(poison) * factor(treatment), data = Poison)
anova(poison.lm)

## ----poison02, fig.show = "hide"-----------------------------------------
plot(poison.lm, w = 1:2)

## ----poison02-fig, echo = FALSE, results = "hide"------------------------
plot(poison.lm, w = 1:2)

## ----poison03, fig.show = "hide"-----------------------------------------
poison.lm2 <- 
  lm(1/time ~ factor(poison) * factor(treatment), data = Poison)
plot(poison.lm2, w = 1:2)

## ----poison-trans-fig, echo = FALSE, results = "hide"--------------------


## ----poison-trans-anova--------------------------------------------------
anova(poison.lm2)

## ----pallets01-----------------------------------------------------------
pallets.lm1 <- lm(pallets ~ employee, data = Pallets)
anova(pallets.lm1)

## ----pallets02-----------------------------------------------------------
pallets.lm2 <- lm(pallets ~ day + employee, data = Pallets)
anova(pallets.lm2)

## ----pallets03, fig.keep = "none"----------------------------------------
xyplot(
  pallets ~ day, data = Pallets,
  groups = employee,
  type="b", auto.key = list(columns = 2, lines = TRUE))

## ----pallets03-fig, tidy = FALSE, echo = FALSE, results = "hide"---------
xyplot(
  pallets ~ day, data = Pallets,
  groups = employee,
  type="b", auto.key = list(columns = 2, lines = TRUE))

## ----pallets04-----------------------------------------------------------
msummary(pallets.lm2)

## ----pallets-perc--------------------------------------------------------
Pallets2 <-
  Pallets %>% 
  group_by(day) %>% mutate(total = sum(pallets)) %>%
  group_by(day, employee) %>% mutate(perc = 100 * pallets / total)

## ----pallets-perc-sol01--------------------------------------------------

## ----palets-perc---------------------------------------------------------
anova(lm(perc ~ employee, data = Pallets2))
confint(glht(lm(perc ~ employee, data = Pallets2), 
             mcp(employee = "Tukey")))

## ----pallets-perc-sol02--------------------------------------------------
anova(lm(perc ~ employee + day, data = Pallets2))
confint(glht(lm(perc ~ employee + day, data = Pallets2), mcp(employee = "Tukey")))
plot(confint(glht(lm(perc ~ employee + day, Pallets2), mcp(employee = "Tukey"))))

## ----domedata------------------------------------------------------------
data(domedata, package = "alr3") 
msummary(domedata)

## ----domedata-step01-----------------------------------------------------
dome.lm1 <- 
  lm(Dist ~ Velocity + Angle + BallWt + BallDia + Cond, data = domedata)
step(dome.lm1, direction = "both", trace = FALSE)

## ----domedata-step02-----------------------------------------------------
step(
  lm(Dist ~ 1, data = domedata),  # starting point
  scope = Dist ~ Velocity + Angle + BallWt + BallDia + Cond, 
  direction = "forward", trace = FALSE)

## ----seatpos-lm----------------------------------------------------------
data(seatpos, package = "faraway")
seatpos.lm1 <- lm(hipcenter ~ ., data = seatpos)
msummary(seatpos.lm1)

## ----seatpos-vif---------------------------------------------------------
faraway::vif(seatpos.lm1)

## ----seatpos-cor, fig.show = "hide"--------------------------------------
car::scatterplotMatrix(
  ~ Age + Arm + hipcenter + Ht + HtShoes + Leg + Seated + Thigh + Weight, 
  data = seatpos,
  reg.line = lm, smooth = TRUE, span = 0.5, 
  diagonal = "density")
round(cor(seatpos), 2)
corrgram::corrgram(seatpos, order = TRUE)

## ----seatpos-cor-fig, echo = FALSE, results = "hide", opts.label = "figbig"----
car::scatterplotMatrix(
  ~ Age + Arm + hipcenter + Ht + HtShoes + Leg + Seated + Thigh + Weight, 
  data = seatpos,
  reg.line = lm, smooth = TRUE, span = 0.5, 
  diagonal = "density")
round(cor(seatpos), 2)
corrgram::corrgram(seatpos, order = TRUE)

## ----seatpos-lm2---------------------------------------------------------
seatpos.lm2 <- lm(hipcenter ~ Age + Weight + Ht, data = seatpos)
msummary(seatpos.lm2)
faraway::vif(seatpos.lm2)

## ----seatpos-pc----------------------------------------------------------
pc = with(seatpos, 
          princomp(cbind(HtShoes, Ht, Seated, Arm, Thigh, Leg),
                   scores = TRUE))
msummary(pc, loadings = TRUE)
seatpos.lmpc <-lm(hipcenter ~ Age + Weight + pc$scores[, 1], data = seatpos)
msummary(seatpos.lmpc)
faraway::vif(seatpos.lmpc)

## ----eval = FALSE--------------------------------------------------------
## x <- 0.65*HtShoes + 0.65*Ht + 0.27*Seated + 0.15*Arm + 0.17*Thigh + 0.18*Leg

## ----seatpos-step--------------------------------------------------------
# trace=0 turns off intermediate reporting
seatpos.lmstep <- step(seatpos.lm1, trace = 0)  
msummary(seatpos.lmstep)
faraway::vif(seatpos.lmstep)

## ----ice01---------------------------------------------------------------
Ice %>% 
  group_by(location, treatment) %>%
  summarise(mean(b1930))

## ----ice02---------------------------------------------------------------
base.lmint <- lm(b1930 ~ location * treatment, data = Ice)
anova(base.lmint)

## ----ice03, fig.show = "hide"--------------------------------------------
base.lmadd <- lm(b1930 ~ location + treatment, data = Ice)
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

## ----ice03-fig, echo = FALSE, results = "hide"---------------------------
base.lmadd <- lm(b1930 ~ location + treatment, data = Ice)
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

## ----ice04---------------------------------------------------------------
require(multcomp) 
confint(glht(base.lmadd, mcp(treatment = "Tukey")), level = 0.9)

## ----ice05---------------------------------------------------------------
ice.trt <- lm(t1930 - b1930 ~ treatment * location, data = Ice)
anova(ice.trt)

## ----ice06---------------------------------------------------------------
ice.trt2 <- lm(t1930 - b1930 ~ treatment, data = Ice,
                 subset = location == "intramuscular")
msummary(ice.trt2)
confint(glht(ice.trt2, mcp(treatment = "Tukey")), level = 0.90)

## ----ice07---------------------------------------------------------------
Ice2 <-
  Ice %>% 
  select(subject, treatment, location, t1930) %>% 
  tidyr::spread(location, t1930) %>% 
  rename(surfTemp = surface, intraTemp = intramuscular)
anova(lm(surfTemp - intraTemp ~ treatment, data = Ice2))

## ----fusion01------------------------------------------------------------
# merge FUSION1 and Pheno keeping only id's that are in both
Fusion1m <- merge(FUSION1, Pheno, by = "id", all = FALSE) 

## ----fusion02------------------------------------------------------------
tally(t2d ~ Gdose, Fusion1m) 

## ----fusion03, tidy = FALSE----------------------------------------------
f1.glm1 <-  
    glm( factor(t2d) ~ Gdose, Fusion1m, family = binomial)
f1.glm1

## ----fusion04------------------------------------------------------------
coef(f1.glm1)
exp(coef(f1.glm1)) 

## ----fusion05------------------------------------------------------------
msummary(f1.glm1) 

## ----fusion06------------------------------------------------------------
1 - pchisq(3231.4 - 3213.0, df = 1)

## ----fusion07------------------------------------------------------------
f1.glm0 <- glm(factor(t2d) ~ 1, Fusion1m, family = binomial) 
deviance(f1.glm0)
deviance(f1.glm1)
df1 <- df.residual(f1.glm0) - df.residual(f1.glm1); df1
1 - pchisq(deviance(f1.glm0) - deviance(f1.glm1), df = df1)

## ----fusion08------------------------------------------------------------
f1.glm2 <- 
    glm(factor(t2d) ~ Gdose + sex, data = Fusion1m, 
        family = binomial())
msummary(f1.glm2)

## ----fusion09------------------------------------------------------------
deviance(f1.glm0)
deviance(f1.glm2)
df2 <- df.residual(f1.glm0) - df.residual(f1.glm2); df2
1 - pchisq(deviance(f1.glm0) - deviance(f1.glm2), df = df2)

## ----step, fig.show = "hide"---------------------------------------------
step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
xyplot(HR - restHR ~ freq, data = Step, groups = height, type = "a")

## ----step-fig, echo = FALSE, results = "hide"----------------------------
step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
xyplot(HR - restHR ~ freq, data = Step, groups = height, type = "a")

## ----rat01, fig.show = "hide"--------------------------------------------
rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
xyplot(consumption ~ flavor, groups = location, data = RatPoison,
       type = c("p", "a"), 
       auto.key = list(points = TRUE, lines = TRUE))

## ----rat01-fig, echo = FALSE, results = "hide"---------------------------
rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
xyplot(consumption ~ flavor, groups = location, data = RatPoison,
       type = c("p", "a"), 
       auto.key = list(points = TRUE, lines = TRUE))

## ----rat02---------------------------------------------------------------
rat.lm1 <- lm(consumption ~ flavor, data = RatPoison)
anova(rat.lm)
anova(rat.lm1)
summary(rat.lm)$sigma
summary(rat.lm1)$sigma
summary(rat.lm1)$sigma^2/summary(rat.lm)$sigma^2

## ----concrete-perm01-----------------------------------------------------
concrete.lm <- lm(strength ~ limestone + water, data = Concrete)

## ----concrete-perm02-----------------------------------------------------
msummary(concrete.lm)
msummary(concrete.lm)$fstat[1] 

## ----concrete-perm03, seed = 12345---------------------------------------
Null.F <- 
  do(5000) * lm(shuffle(strength) ~ limestone + water, data = Concrete)
Null.F %>% head(3)
prop( ~ (F > summary(concrete.lm)$fstat[1]), data = Null.F) 

## ----concrete-perm04, cache = TRUE, seed = 12345-------------------------
Null.t2 <- 
  do(10000) * 
    lm(strength ~ shuffle(limestone) + water, data = Concrete) %>%
    coef() %>% getElement(2)
  
Null.t3 <- 
  do(10000) * {
      lm(strength ~ limestone + shuffle(water), data = Concrete) %>%
      coef() %>% getElement(3)
  }
Null.t2 %>% head(3)
2 * prop( ~ (result >= coef(concrete.lm)[2]), data = Null.t2)
2 * prop( ~ (result <= coef(concrete.lm)[3]), data = Null.t3)

## ----concrete-perm-sol---------------------------------------------------
histogram( ~ Estimate | factor(.row), data = Null.t, scales = "free",
           v = c(0, mean(~strength, data = Concrete)))

## ----smoke-perm, cache = TRUE--------------------------------------------
tally( ~ student + parents, data = FamilySmoking, margin = FALSE) -> smokeTab; 
smokeTab
chisq.test(smokeTab)
observedStat <- chisq.test(smokeTab)$stat
stats <- do(2000) * 
  stat(chisq.test(tally( ~ shuffle(student) + parents, 
                         data = FamilySmoking, margin = FALSE)))
stats <- stats$X.squared
sum(stats >= observedStat) -> x; x / length(stats)   # p-value
binom.test(x, length(stats), alternative = "less") %>% confint()

## ----concrete-anova------------------------------------------------------
anova(lm(strength ~ limestone * water, data = Concrete))
anova(lm(strength ~ water * limestone, data = Concrete))
anova(lm(strength ~ limestone:water + limestone + water, data = Concrete))
anova(lm(strength ~ limestone:water +  water + limestone, data = Concrete))

## ----rat-liver-----------------------------------------------------------
data(rat, package = "alr3")  
rat.lm <- lm(y ~ BodyWt * LiverWt, data = rat)
msummary(rat.lm)

## ----rabbit-table, eval = FALSE------------------------------------------
## data(rabbit, package = "faraway")
## tally( ~ treat + block, data = rabbit)
## xtabs(gain ~ treat + block, data = rabbit)

## ----rabbit--------------------------------------------------------------
data(rabbit, package = "faraway")
rabbit.lm1 <- lm(gain ~ block + treat, data = rabbit)
rabbit.lm2 <- lm(gain ~ treat + block, data = rabbit)
rabbit.lm3 <- lm(gain ~ block, data = rabbit)
rabbit.lm4 <- lm(gain ~ treat, data = rabbit)

## ------------------------------------------------------------------------
anova(rabbit.lm1)
anova(rabbit.lm1, rabbit.lm3)

## ------------------------------------------------------------------------
anova(rabbit.lm2)
anova(rabbit.lm2, rabbit.lm4)

## ------------------------------------------------------------------------
# width of Tukey HSD intervals:
2 * qtukey(0.95, 6, 15) / sqrt(5) * summary(rabbit.lm1)$sigma
# TukeyHSD() can automate this:
TukeyHSD(aov(gain ~ block + treat, data = rabbit), "treat")

## ----eggprod-------------------------------------------------------------
data(eggprod, package = "faraway")
eggprod.lm <- lm(eggs ~ block + treat, data = eggprod)
anova(eggprod.lm)

## ------------------------------------------------------------------------
msummary(eggprod.lm)
coef(msummary(eggprod.lm))

## ------------------------------------------------------------------------
eggprod.lm1way <- lm(eggs ~ treat, data = eggprod)
anova(eggprod.lm1way)
# width of Tukey HSD intervals:
2 * qtukey(0.95, 3, 6) / sqrt(4) * msummary(eggprod.lm)$sigma
# TukeyHSD() can automate this:
TukeyHSD(aov(eggs ~ block + treat, eggprod), "treat")

## ------------------------------------------------------------------------
y <- AirPollution$pollution
model <- lm(pollution ~ location, data = AirPollution)
DFE <- anova(model)["Residuals", "Df"]
SSE <- anova(model)["Residuals", "Sum Sq"]
MSE <- anova(model)["Residuals", "Mean Sq"]
s <- sqrt(MSE); s

## ------------------------------------------------------------------------
v <- c(1/2, 1/2, 2/2, 2/2, -3/2, -3/2)
kappa <- 1 / vlength(v); kappa
u <- v / vlength(v); u
dot(y, u)
F <- dot(y, u)^2 / MSE; F
1 - pf(F, 1, DFE)                # p-value

## ------------------------------------------------------------------------
C_hat <- sum( c(1, 2, -3) * mean(pollution ~ location, data = AirPollution)); C_hat
t <- (C_hat - C) / (s/kappa)
2 * pt( - abs(t), df = DFE)

## ------------------------------------------------------------------------
### model comparison
AirPollution <- 
  AirPollution %>%
  mutate(
    x0 = as.numeric(location == levels(location)[1]),
    x1 = as.numeric(location == levels(location)[2]),
    x2 = as.numeric(location == levels(location)[3])
  )
AirPollution
# using mu1 = 3 * mu3 - 2 * mu2
model2 <- lm(pollution ~ 0 + I(x1 - 2*x0) + I(x2 + 3*x0), data = AirPollution)
anova(model2, model)
# using beta_1 = 3/2 * beta_2
model3 <- lm(pollution ~ 1 + I(1.5 * x1 + x2), data = AirPollution)
anova(model3, model)

## ----include = FALSE-----------------------------------------------------
data(AirPollution)  # restore data to orignal form


## ----SeeAlso, child="SeeAlso.Rnw", eval=TRUE-----------------------------



## ----RIntro, child="RIntro.Rnw", eval=includeApp[1]----------------------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/R-") 

## ----install-tidyr, eval = FALSE-----------------------------------------
## install.packages("tidyr") # fetch package from CRAN to local machine.
## require(tidyr)            # load the package so it can be used.

## ----install-tidyr-lib, eval = FALSE-------------------------------------
## install.packages("tidyr", lib = "~/R/library")

## ----install-github, eval = FALSE----------------------------------------
## # install.packages("devtools")
## devtools::install_github("hadley/dplyr", build_vignettes = FALSE)
## devtools::install_github("hadley/lazyeval", build_vignettes = FALSE)
## devtools::install_github("rstudio/ggvis", build_vignettes = FALSE)

## ----fastR2-github, eval = FALSE-----------------------------------------
## install_github("rpruim/fastR2")

## ----eval = FALSE--------------------------------------------------------
## install.packages("some-package.tar.gz",
##                   repos = NULL)           # use a file, not a repository

## ----eval = FALSE--------------------------------------------------------
## ?Startup

## ----eval = FALSE--------------------------------------------------------
## # always load my favorite packages
## require(fastR2)
## # adjust lattice settings
## trellis.par.set(theme = col.fastR())

## ----eval = FALSE--------------------------------------------------------
## TEXINPUTS=:.:./inputs//:$TEXINPUTS
## TEXINPUTS=$TEXINPUTS:/usr/local/texlive/2016/texmf-dist/tex//
## R_PDFLATEXCMD=/Library/TeX/texbin/pdflatex
## R_PAPERSIZE=letter
## R_PDFVIEWER="/usr/bin/open -a skim"

## ----search--------------------------------------------------------------
search()
find("logit")

## ----logit-faraway-------------------------------------------------------
 mosaic::logit(0.3)
faraway::logit(0.3)
    car::logit(0.3)
# using percent rather than proportion -- only works in the car version
    car::logit(30)
faraway::logit(30)         
 mosaic::logit(30)         

## ----data-package01------------------------------------------------------
data(Traffic, package = "MASS")
head(Traffic)
data(Traffic, package = "fastR2")
head(Traffic)

## ----data-package02------------------------------------------------------
head(MASS::Traffic)
head(fastR2::Traffic)

## ----help, eval = FALSE--------------------------------------------------
## ?c
## ?"for"

## ----apropos-------------------------------------------------------------
apropos("hist")

## ----args----------------------------------------------------------------
args(require)
args(mean)

## ----usage---------------------------------------------------------------
formatR::usage(sum)

## ----example, eval = FALSE-----------------------------------------------
## example(histogram)

## ----savehistory, eval = FALSE-------------------------------------------
## savehistory("someRCommandsIalmostLost.R")

## ----packageData-iris----------------------------------------------------
# first line only necessary if iris is already in use 
data(iris)         
str(iris)           # get a summary of the data set
dim(iris)           # just the dimensions
glimpse(iris)       # take a quick look at the data
inspect(iris)       # another quick look at the data

## ----read-table----------------------------------------------------------
# need header = TRUE because there is a header line.
# could also use read.file() without header = TRUE
Traffic <- 
    read.table("http://www.calvin.edu/~rpruim/fastR/trafficTufte.txt", 
    header = TRUE)
Traffic

## ----read-sas------------------------------------------------------------
traffic <- 
    read.csv("http://www.calvin.edu/~rpruim/fastR/trafficTufte.csv", 
    na.strings = c(".", "NA", ""))

## ----scan01, eval = FALSE------------------------------------------------
## myData1 <- scan()

## ----include = FALSE-----------------------------------------------------
myData1 <- c(15, 18, 12, 21, 23, 50, 15)

## ----scan02--------------------------------------------------------------
myData1

## ----scan03, eval = FALSE------------------------------------------------
## myData2 <- scan(what = "character")

## ----include = FALSE-----------------------------------------------------
myData2 <- c("red", "red", "orange", "green", "blue", "blue", "red")

## ----scan04--------------------------------------------------------------
myData2

## ----dataframe-----------------------------------------------------------
myDataFrame <- data.frame(color = myData2, number = myData1)
myDataFrame

## ----generating-data01---------------------------------------------------
x <- 5:20; x                 # all integers in a range
# structured sequences
seq(0, 50, by = 5)               
seq(0, 50, length = 7)               
rep(1:5, each = 3)
rep(1:5, times = 3)
c(1:5, 10, 3:5)              # c() concatenates 

## ----generating-data02, seed = 1234, fig.keep = "none"-------------------
rnorm(10, mean = 10, sd = 2)  # random normal draws
histogram( ~ rnorm(1000, mean = 10, sd = 2))

## ----generating-data02-fig, echo = FALSE, results = "hide"---------------
rnorm(10, mean = 10, sd = 2)  # random normal draws
histogram( ~ rnorm(1000, mean = 10, sd = 2))

## ----generating-data03, seed = 12345-------------------------------------
sample(Births78, 3)       # sample 3 rows from Births78
Births78 %>% sample(3)    # sample 3 rows from Births78
sample(1:10, size = 5)    # random sample of size 5 (w/o replacement)
resample(1:10, size = 5)  # random sample of size 5 (w/ replacement)

## ----writingData---------------------------------------------------------
args(write.table)
SomeData <- data.frame(x = 1:3, y = LETTERS[1:3])
SomeData
write.table(SomeData, "SomeData.txt")
write.csv(SomeData, "SomeData.csv")
# this system call should work on a Mac or Linux machine
system("head SomeData.txt SomeData.csv")

## ----savingData----------------------------------------------------------
greeting <- "hello, world!"
save(SomeData, greeting, file = "mystuff.rda")  # saves both objects in 1 file
load("mystuff.rda")                             # loads them both

## ------------------------------------------------------------------------
saveRDS(SomeData, file = "SomeData.rds")
EEE <- readRDS("SomeData.rds")
EEE

## ----births-cumsum, fig.keep = "none"------------------------------------
data(Births78)
Births78 <- 
  mutate(Births78, runningTotal = cumsum(births))
head(Births78, 3)
xyplot(runningTotal ~ date, data = Births78, type = "l") 

## ----births-cumsum-fig, echo = FALSE, results = "hide"-------------------
data(Births78)
Births78 <- 
  mutate(Births78, runningTotal = cumsum(births))
head(Births78, 3)
xyplot(runningTotal ~ date, data = Births78, type = "l") 

## ----cps85-mutate--------------------------------------------------------
CPS85 <- mutate(CPS85, workforce.years = age - 6 - educ)
favstats( ~ workforce.years, data = CPS85)

## ----CPS85-tally---------------------------------------------------------
tally( ~ (exper - workforce.years), data = CPS85)

## ----tidy = FALSE--------------------------------------------------------
HELP2 <- mutate( HELPrct, 
  newsex = factor(female, labels = c("M", "F")) )

## ----HELP2-tally---------------------------------------------------------
tally( ~ newsex + female, data = HELP2 )

## ----derivedFactor, tidy = FALSE, fig.keep = "none"----------------------
HELP3 <- mutate(HELPrct, 
  risklevel = derivedFactor(
    low = sexrisk < 5, 
	medium = sexrisk < 10,
	high = sexrisk >= 10,
	.method = "first"      # use first rule that applies
	)
)
xyplot(sexrisk ~ risklevel, data = HELP3, jitter.x = TRUE, alpha = 0.4)

## ----derivedFactor-fig, echo = FALSE, results = "hide"-------------------
HELP3 <- mutate(HELPrct, 
  risklevel = derivedFactor(
    low = sexrisk < 5, 
	medium = sexrisk < 10,
	high = sexrisk >= 10,
	.method = "first"      # use first rule that applies
	)
)
xyplot(sexrisk ~ risklevel, data = HELP3, jitter.x = TRUE, alpha = 0.4)

## ----select01------------------------------------------------------------
CPS1 <- select(CPS85, - workforce.years)
head(CPS1, 2)

## ----select02------------------------------------------------------------
CPS2 <- select(CPS85, workforce.years, exper)
head(CPS2, 2)

## ----select03------------------------------------------------------------
CPSsmall <- select(CPS85, 1:4)
head(CPSsmall, 2)

## ----select04------------------------------------------------------------
head(select(HELPrct, contains("risk")), 2)

## ----chain01-------------------------------------------------------------
HELPrct %>% select(contains("risk")) %>% head(2)

## ----eval = FALSE--------------------------------------------------------
## bop(
##   scoop(
##     hop(foo_foo, through = forest),
##     up = field_mice
##   ),
##   on = head
## )

## ----eval = FALSE--------------------------------------------------------
## foo_foo %>%
##   hop(through = forest) %>%
##   scoop(up = field_mouse) %>%
##   bop(on = head)

## ----chain-2, tidy = FALSE-----------------------------------------------
HELPrct %>% 
  select(ends_with("e")) %>% 
  head(2)
HELPrct %>% 
  select(starts_with("h")) %>% 
  head(2)
HELPrct %>% 
  select(matches("i[12]")) %>% # regex matching
  head(2)  

## ----faithful-names------------------------------------------------------
names(faithful)

## ----names01-------------------------------------------------------------
names(faithful) <- c("duration", "time_til_next")
head(faithful, 3)

## ----data-revert, eval = TRUE--------------------------------------------
# don't execute this unless you want to revert to the original data
data(faithful)  

## ----rename01------------------------------------------------------------
faithful <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)

## ----faithful-xy---------------------------------------------------------
xyplot(time_til_next ~ duration, data = faithful)

## ----rename02------------------------------------------------------------
CPS85 %>% 
  rename(education = educ) %>%
  head(4)

## ----tidy = FALSE--------------------------------------------------------
CPS85 %>% 
  select(education = educ, wage, race) %>%
  head(3)

## ----faithful-long, fig.keep = "none"------------------------------------
# any logical can be used to create subsets
data(faithful)
faithfulLong <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting) %>%
  filter(duration > 3) 
xyplot(time_til_next ~ duration, data = faithfulLong)

## ----faithful-log-fig, echo = FALSE, results = "hide"--------------------
# any logical can be used to create subsets
data(faithful)
faithfulLong <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting) %>%
  filter(duration > 3) 
xyplot(time_til_next ~ duration, data = faithfulLong)

## ----faithful-long-alt, eval = FALSE, tidy = FALSE, fig.keep = "last", fig.show = "hide"----
## xyplot(time_til_next ~ duration,
##        data = faithfulLong %>% filter( duration > 3))
## xyplot(time_til_next ~ duration, data = faithfulLong,
##        subset = duration > 3)
## xyplot(time_til_next ~ duration, data = faithfulLong, xlim = c(3, NA))

## ----summarise01---------------------------------------------------------
HELPrct %>% 
  summarise(x.bar = mean(age), s = sd(age))

## ----summarise-group_by--------------------------------------------------
HELPrct %>% 
  group_by(sex, substance) %>%
  summarise(x.bar = mean(age), s = sd(age))

## ----dplyr-vs-mosaic-----------------------------------------------------
favstats(age ~ sex + substance, data = HELPrct)
    mean(age ~ sex + substance, data = HELPrct, .format = "table")
      sd(age ~ sex + substance, data = HELPrct, .format = "table")

## ----arrange, tidy = FALSE-----------------------------------------------
HELPrct %>% 
  group_by(sex, substance) %>%
  summarise(x.bar = mean(age), s = sd(age)) %>% 
  arrange(x.bar)

## ----FUSION-Pheno--------------------------------------------------------
head(FUSION1, 3)
head(Pheno, 3)

## ----merge-join, tidy = FALSE--------------------------------------------
# merge FUSION1 and Pheno keeping only id's that are in both
FUSION1m <- merge(FUSION1, Pheno, by.x = "id", by.y = "id", 
                  all.x = FALSE, all.y = FALSE)
head(FUSION1m, 3)
left_join(Pheno, FUSION1, by = "id") %>% dim()
inner_join( Pheno, FUSION1, by = "id") %>% dim()
# which ids are only in Pheno?
setdiff(Pheno$id, FUSION1$id)   
anti_join(Pheno, FUSION1)

## ----fusion1-xtabs-------------------------------------------------------
tally( ~ t2d + genotype + marker, data = FUSION1m)

## ----mergin-sol----------------------------------------------------------
FUSION1m2 <- 
  FUSION1 %>%
  merge(Pheno, by.x = "id", by.y = "id", all.x = FALSE, all.y = FALSE)
Pheno %>%  left_join(FUSION1, by = "id") %>% dim()
Pheno %>% inner_join(FUSION1, by = "id") %>% dim()

## ----FUSION-names--------------------------------------------------------
names(FUSION1)
names(FUSION2)

## ----births-range--------------------------------------------------------
data(Births)
range( ~ date, data = Births)

## ----births-sol01--------------------------------------------------------
Births %>% 
  group_by(moth, day) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) %>% head(4)
Births %>% 
  group_by(month, day) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(bpd) %>% head(4)

## ----births-sol02--------------------------------------------------------
Births %>% 
  group_by(month) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) %>% head(3)
Births %>% 
  group_by(month) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(bpd) %>% head(3)

## ----births-sol03--------------------------------------------------------
Births %>% 
  group_by(wday) %>%
  summarise(bpd = mean(births)) %>% 
  arrange(-bpd) 

## ----Utilities-temp------------------------------------------------------
Temp <-
  Utilities %>% 
  select(month, year, temp) 
head(Temp, 3)

## ----Temp-spread---------------------------------------------------------
require(tidyr) 
Temp2 <- 
  Temp %>%
  spread(key = month, value = temp)
Temp2

## ----Temp-gather---------------------------------------------------------
Temp3 <- 
  Temp2 %>%
  gather(key = month, value = temp, `1` : `12`)  
Temp3 %>% head

## ----Temp-gather-numeric-------------------------------------------------
Temp3a <- 
  Temp2 %>%
  gather(key = month, value = temp, 2 : 13)  
Temp3a %>% head

## ----tidy = FALSE, warning = FALSE---------------------------------------
# connect to a UCSC database
UCSCdata <- src_mysql(
  host = "genome-mysql.cse.ucsc.edu",
  user = "genome",
  dbname = "mm9")
# grab one of the many tables in the database
KnownGene <- tbl(UCSCdata, "knownGene")

# Get the gene name, chromosome, start and end sites for genes on Chromosome 1
Chrom1 <-
  KnownGene %>% 
  select( name, chrom, txStart, txEnd ) %>%
  filter( chrom == "chr1" )

## ------------------------------------------------------------------------
class(Chrom1)

## ----Chrom1l, tidy = FALSE-----------------------------------------------
Chrom1l <-
  Chrom1 %>% mutate(length = (txEnd - txStart)/1000)
Chrom1l

## ----tidy = FALSE--------------------------------------------------------
Chrom1df <- collect(Chrom1l)       # collect into a data frame
histogram( ~ length, data = Chrom1df, xlab = "gene length (kb)")

## ----mode01--------------------------------------------------------------
w <- 2.5; mode(w); length(w)
x <- c(1, 2); mode(x); length(x)
y <- "foo"; mode(y); length(y)
y[1]; y[2]             # not an error to ask for y[2]
z <- TRUE; mode(z); length(z)
abc <- letters[1:3]
abc; mode(abc); length(abc)
abc[3]
abc[6] <- "Z"; abc     # NAs fill in to make vector long enough

## ------------------------------------------------------------------------
L <- list(a = 5, b = "X", c = 2.3, d = c("A", "B")); mode(L); length(L)
L[[4]]        # 4th element of list
L[["d"]]      # element named "d"
L$d           # element named "d"
L[["d"]] <- 1:3; str(L)
L[["b"]] <- NULL; str(L)     # removing an item from a list

## ----access--------------------------------------------------------------
xm <- matrix(1:16, nrow = 4); xm
xm[5]
xm[, 2]                   # this is 1 dimensional (a vector)
xm[, 2, drop = FALSE]     # this is 2 dimensional (still a matrix)

## ----rows-and-cols-------------------------------------------------------
DDD <- data.frame(number = 1:5, letter = letters[1:5])
dim(DDD)
nrow(DDD)
ncol(DDD)
names(DDD)
row.names(DDD)
row.names(DDD) <- c("Abe", "Betty", "Claire", "Don", "Ethel")
DDD                 # row.names affects how a data.frame prints

## ------------------------------------------------------------------------
attributes(DDD)

## ----what-is-it----------------------------------------------------------
xm <- matrix(1:16, nrow = 4); xm
mode(xm); class(xm)
c(is.numeric(xm), is.character(xm), is.integer(xm), is.logical(xm))
c(is.vector(xm), is.matrix(xm), is.array(xm))

## ----as-you-like-it------------------------------------------------------
apropos("^as\\.")[1:10]      # just a small sample
# convert numbers to strings (this drops attributes)
as.character(xm)             
# convert matrix to vector
as.vector(xm)
as.logical(xm)
alpha <- c("a", "1", "b", "0.5")    
mode(alpha)
as.numeric(alpha)      # can't do the coersion, so NAs are introduced
as.integer(alpha)      # notice coersion of 0.5 to 0

## ----vectors01-----------------------------------------------------------
x <- 1:5; y <- seq(10, 60, by = 10); z <- rnorm(10); x; y
y + 1
x * 10
x < 3
x^2
log(x); log(x, base = 10)            # natural and base 10 logs

## ----vectors02-----------------------------------------------------------
# compare round() and signif() by binding rowwise into matrix
rbind(round(z, digits = 2), signif(z, digits = 2))   

## ----vectors03-----------------------------------------------------------
x <- 1:10; z <- rnorm(100)
mean(z); sd(z); var(z); median(z)  # basic statistical functions
range(z)                           # range returns a vector of length 2

## ----vectors04-----------------------------------------------------------
sum(x); prod(x)                         # sums and products
z <- rnorm(5); z
sort(z); rank(z); order(z)              # sort, rank, order
rev(x)                                  # reverse x
diff(x)                                 # pairwise differences
cumsum(x)                               # cumulative sum
cumprod(x)                              # cumulative product

## ----vectors05-----------------------------------------------------------
x <- 1:5; y <- seq(10, 70, by = 10)
x + y

## ----vectors06-----------------------------------------------------------
x <- seq(2, 20, by = 2)
x[1:5]; x[c(1, 4, 7)]

## ----vectors07-----------------------------------------------------------
x <- seq(2, 20, by = 2)
x[c(TRUE, TRUE, FALSE)]      # skips every third element (recycling!)
x[x > 10]                    # more typical use of boolean in selection

## ----vectors08-----------------------------------------------------------
x <- 1:10; x[-7]; x[-c(1, 2, 4, 8)]; x[-length(x)]

## ----vectors09-----------------------------------------------------------
notes <- toupper(letters[1:7]); a <- 1:5; b <- seq(10, 100, by = 10)
toupper(letters[5:10])                
paste(letters[1:5], 1:3, sep = "-")
a+b
(a+b)[ a+b > 50]
length((a+b)[a+b > 50])
table(a+b > 50)

## ----function01----------------------------------------------------------
fstats <- function(x) {
    mean(x)
    median(x)
    sd(x)
}

fstats((1:20)^2)

## ----defFun02------------------------------------------------------------
fstats <- function(x) {
    print(mean(x))
    print(median(x))
    print(sd(x))
}

fstats((1:20)^2)

## ----defFun02-cat--------------------------------------------------------
altfstats <- function(x) {
    cat(paste("  mean:", format(mean(x), 4), "\n"))
    cat(paste(" edian:", format(median(x), 4), "\n"))
    cat(paste("    sd:", format(sd(x), 4), "\n"))
}
altfstats((1:20)^2)

## ----defFun02a-----------------------------------------------------------
temp <- fstats((1:20)^2)
temp

## ----defFun03------------------------------------------------------------
fstats <- function(x) {
	c(mean(x), median(x), sd(x))
}
fstats((1:20)^2)

## ----defFun04------------------------------------------------------------
fstats <- function(x) {
	result <- c(min(x), max(x), mean(x), median(x), sd(x))
    names(result) <- c("min", "max", "mean", "median", "sd")
    return(result)
}
fstats((1:20)^2)

## ----apply---------------------------------------------------------------
sapply(KidsFeet, class)    # determine the class of each variable
lapply(iris, function(x) if (is.numeric(x)) favstats(x) else tally(x))
M <- rbind(1:3, 4:6, 7:9); M
apply(M, 1, sum)           # row sums
rowSums(M)                 # dedicated row sums function
# tapply version of mean(length ~ sex, data = KidsFeet)
tapply(KidsFeet$length, KidsFeet$sex, mean)

## ----iris-splom, fig.show = "hide"---------------------------------------
splom(iris)

## ----iris-splom-fig, echo = FALSE, opts.label = "figbig"-----------------
splom(iris)

## ----iris-parallel, fig.show = "hide"------------------------------------
parallel(~ iris[1:4] | Species, data = iris)
set.seed(123)
iris.sample <- iris[sample(1:(dim(iris)[1]), 15), ]
parallel(~ iris.sample[1:4] | Species, data = iris.sample)

## ----iris-parallel-fig, echo = FALSE-------------------------------------
parallel(~ iris[1:4] | Species, data = iris)
set.seed(123)
iris.sample <- iris[sample(1:(dim(iris)[1]), 15), ]
parallel(~ iris.sample[1:4] | Species, data = iris.sample)

## ----xyplot-type, eval = FALSE-------------------------------------------
## xyplot(y ~ x , type = "l")

## ----iris-splom02, fig.show = "hide"-------------------------------------
splom(~iris[1:4], data = iris, groups = Species,
    pch = c(21, 23, 25), 
    cex = 0.8,
    alpha = 0.7,
    col = "gray50",
    fill = trellis.par.get("superpose.symbol")$col
    )

## ----iris-splom02-fig, echo = FALSE, opts.label = "figbig"---------------
splom(~iris[1:4], data = iris, groups = Species,
    pch = c(21, 23, 25), 
    cex = 0.8,
    alpha = 0.7,
    col = "gray50",
    fill = trellis.par.get("superpose.symbol")$col
    )

## ----set-pch-------------------------------------------------------------
trellis.par.get("plot.symbol")
# set plot symbol default to be a blue diamond with black border
trellis.par.set(plot.symbol = list(pch = 23, col = "black", fill = "lightblue"))  
trellis.par.get("plot.symbol")

## ----lattice-settings01--------------------------------------------------
trellis.par.set(theme = col.whitebg())

## ----lattice-settings02, eval = FALSE------------------------------------
## trellis.par.get()

## ----fastR-theme---------------------------------------------------------
trellis.par.set(theme = col.fastR())

## ----mytheme, include = FALSE--------------------------------------------
mytheme <- function() {
    list(background = list(col = "transparent"), 
        plot.polygon = list(col = "navy"), 
        box.rectangle = list(col = "navy"), 
        box.umbrella = list(col = "navy"), 
        dot.line = list(col = "#e8e8e8"), 
        dot.symbol = list(col = "navy", pch = 16), 
        plot.line = list(col = "navy", lwd = 2), 
        plot.symbol = list(col = "navy", pch = 16), 
        regions = list(col = heat.colors(100)), 
        reference.line = list(col = "#e8e8e8"), 
        superpose.line = list(lty = 1:7,
                    col = c("navy", "red", "darkgreen", "turquoise", "orange",
                        "purple", "pink", "lightgreen")),
        superpose.symbol = list(pch = c(16, 1, 3, 6, 0, 5, 17), 
            cex = rep(0.7, 7), 
            col = c("navy", "red", "darkgreen", "turquoise", "orange",
                        "purple", "pink", "lightgreen")),
        strip.background = list(alpha = 1,
           col = c("#ffe5cc", "#ccffff",
                 "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")
            ),
        strip.shingle = list(alpha = 1,
            col = c("#ff7f00", "#00ffff",
                 "#0080ff", "#ff00ff", "#ff0000", "#ffff00"))
        )
}

## ----lattice-settings03, opts.label = "figbig", fig.keep = "none"--------
show.settings()

## ----lattice-settings03-fig, opts.label = "figbig", echo = FALSE, results = "hide"----
show.settings()

## ----defFun11------------------------------------------------------------
x = (0:10)/10
myData = data.frame(x = x, y = sin(x))
panel.xyplotWithDiag <- function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(a = 0, b = 1, col = "gray30", lwd = 2)
}

## ----defFun12, fig.show = "hide"-----------------------------------------
xyplot(y~x, data = myData, panel = panel.xyplotWithDiag)
panel.xyplotWithLine <- function(x, y, intercept = 0, slope = 1, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(a = intercept, b = slope, ...)
}
xyplot(y~x, data = myData, panel = panel.xyplotWithLine)
xyplot(y~x, data = myData, 
    inter = 0.5, slope = 0, pch = 16,
    lwd = 2, col = "gray30", lty = 2,
    panel = panel.xyplotWithLine,
)

## ----defFun12-fig, echo = FALSE------------------------------------------
xyplot(y~x, data = myData, panel = panel.xyplotWithDiag)
panel.xyplotWithLine <- function(x, y, intercept = 0, slope = 1, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(a = intercept, b = slope, ...)
}
xyplot(y~x, data = myData, panel = panel.xyplotWithLine)
xyplot(y~x, data = myData, 
    inter = 0.5, slope = 0, pch = 16,
    lwd = 2, col = "gray30", lty = 2,
    panel = panel.xyplotWithLine,
)

## ----histogram-rug-density, fig.show = "hide"----------------------------
x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
            }
     )

## ----histogram-rug-density-grid, fig.show = "hide"-----------------------
x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
                grid::grid.text("Look Here",
                    x = 0.5, y = 0.48,
                    just = "left",
                    default.units = "npc",
                    rot = 33,
                    gp = grid::gpar(col = "black", cex = 2)
                    )
                grid::grid.segments( 
                    x0 = 0.48, x1 = unit(-0.92, "native"),
                    y0 = 0.45, y1 = unit(0.085, "native"),
                    arrow = grid::arrow(),           # default arrow
                    default.units = "npc",
                    gp = grid::gpar(col = "black")
                    )
                grid::grid.rect(x = -2, y = 0, width = 1, height = 0.15, 
                    default.units = "native",
                    just = c("left", "bottom"), 
                    gp = grid::gpar(
                      col = "black", fill = "gray40", alpha = 0.6)
                    )
            }
     )

## ----histogram-rug-fig, echo = FALSE-------------------------------------
x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
            }
     )
x <- rnorm(100)
histogram(~x, type = "density",
            panel = function(x, y, ...) {
                panel.rug(x, ...)
                panel.histogram(x, ...)
                panel.mathdensity(
                    dmath = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    lwd = 5, col = "black", lty = 1, alpha = 0.5,
                    ...)
                grid::grid.text("Look Here",
                    x = 0.5, y = 0.48,
                    just = "left",
                    default.units = "npc",
                    rot = 33,
                    gp = grid::gpar(col = "black", cex = 2)
                    )
                grid::grid.segments( 
                    x0 = 0.48, x1 = unit(-0.92, "native"),
                    y0 = 0.45, y1 = unit(0.085, "native"),
                    arrow = grid::arrow(),           # default arrow
                    default.units = "npc",
                    gp = grid::gpar(col = "black")
                    )
                grid::grid.rect(x = -2, y = 0, width = 1, height = 0.15, 
                    default.units = "native",
                    just = c("left", "bottom"), 
                    gp = grid::gpar(
                      col = "black", fill = "gray40", alpha = 0.6)
                    )
            }
     )

## ----whats-up, eval = FALSE----------------------------------------------
## odds <- 1 + 2 * (0:4)
## primes <- c(2, 3, 5, 7, 11, 13)
## length(odds)
## length(primes)
## odds + 1
## odds + primes
## odds * primes
## odds > 5
## sum(odds > 5)
## sum(primes < 5 | primes > 9)
## odds[3]
## odds[10]
## odds[-3]
## primes[odds]
## primes[primes >= 7]
## sum(primes[primes > 5])
## sum(odds[odds > 5])
## odds[10] <- 1 + 2 * 9
## odds
## y <- 1:10
## (x <- 1:5)

## ----whats-up-sol--------------------------------------------------------

## ----chickwt-sol01-------------------------------------------------------
ChickWeight %>% 
  filter(Time == 21) %>%
  arrange(weight) %>%
  head(1)
ChickWeight %>% 
  filter(Time == 21) %>%
  arrange(weight) %>%
  tail(1)

## ----chickwt-sol02-------------------------------------------------------
Chicks <- 
  ChickWeight %>% 
  filter(Time > 15) %>%   # remove chicks that were only measured a few times
  group_by(Chick) %>%
  summarise(
    weight = max(weight),
    diet = Diet[1],
    time = max(Time)
    ) %>%
  ungroup() %>%     # need this for arrange to work properly
  arrange(weight) 
Chicks %>% head(1)
Chicks %>% tail(1)


## ----MathNotation, child="MathNotation.Rnw", eval=includeApp[2]----------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Math-")

## ----sum-ssols-----------------------------------------------------------
(2:5)^2 
sum( (2:5)^2 )

## ----some-sums-----------------------------------------------------------
x <- 0:4;
p <- c(1/6,1/3,1/4,1/6,1/12);
sum(p);
sum(p*x);
fractions( sum(p*x) );
sum(p*x^2);
fractions( sum(p*x^2) );


## ----LinearAlgebra, child="LinearAlgebra.Rnw", eval=includeApp[3]--------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/LA-")

## ----vec-mult01----------------------------------------------------------
x <- c(1, 2, 3)
4 * x

## ----vec-mult02----------------------------------------------------------
u <- c(1, 2, 3)
v <- c(4, 5, 6)
u * v

## ----vec-dot-------------------------------------------------------------
dot(u, v)

## ----vlength-def---------------------------------------------------------
vlength

## ----length-vlength------------------------------------------------------
x <- 1:5
length(x)
vlength(x)

## ----vec-proj, tidy = FALSE----------------------------------------------
x <- c(1, 2, 3); v <- c(1, 1, 1)
project(x, v) 
dot(x, v) * v / vlength(v)^2
project(x, v, type = 'coef') 
dot(x, v) / vlength(v)^2
project(x, v, type = 'length') 
dot(x, v) / vlength(v)

## ----vdecomp2------------------------------------------------------------
vdecomp2 <- function(x, v1, v2) {
  w1 <- v1 - project(v1, v2); w2 <- v2 - project(v2, v1) 
  p1 <- project(x, w1);       p2 <- project(x, w2) 
  # need to be careful about 0 vectors
  a  <- if (vlength(w1) == 0) { 0 } else {
    sign(dot(w1, p1)) * vlength(p1) / vlength(w1)
  }
  b  <- if (vlength(w2) == 0) { 0 } else { 
    sign(dot(w2, p2)) * vlength(p2) / vlength(w2)
  }
  list( 
    coefficients = c(a, b), 
    projection   = a * v1 + b * v2,
    remainder    = x - a * v1 - b * v2
  )
}

## ----vector-decomp01-----------------------------------------------------
v1 <- c(1, 1)
v2 <- c(2, 3)
x  <- c(2, 5)
vdecomp2(x, v1, v2)

## ----vector-decomp02-----------------------------------------------------
v1 <- c(1, 0, 0)
v2 <- c(1, 1, 1)
x  <- c(2, 3, 5)
vdecomp2(x, v1, v2)
h <- vdecomp2(x, v1, v2)$remainder; 
round(h, 8)
round(dot(h, v1), 8)
round(dot(h, v2), 8)

## ----vector-decomp03-----------------------------------------------------
v1 <- c(1, 0, 0)
v2 <- c(1, 1, 1)
v3 <- c(1, 2, 3)
x  <- c(2, 7, 3)
vdecomp2(x, v1, v2) %>% lapply(round, digits = 8)
a1 <- vdecomp2(x, v1, v2)$coefficients[1]; a1
b1 <- vdecomp2(x, v1, v2)$coefficients[2]; b1
x1 <- a1 * v1 + b1 * v2; x1
# decompose x into x1 and v3 
vdecomp2(x, x1, v3) %>% lapply(round, digits = 8)
a2 <- vdecomp2(x, x1, v3)$coefficients[1]; a2
b2 <- vdecomp2(x, x1, v3)$coefficients[2]; b2
# this should equal x
a2 * (a1 * v1 + b1* v2) + b2 * v3 
# the three coefficients
c(a2 * a1, a2 * b1, b2)

## ----vector-decomp04, tidy = FALSE---------------------------------------
vdecomp <- function(x, ...) {
  v <- list(...)
  projection <- project(x, v[[1]])
  coefs <-project(x, v[[1]], type = "coef")
  for (i in 2:length(v)) {
    decomp <- vdecomp2(x, projection, v[[i]])
    coefs <- c(coefs * decomp$coefficients[1], decomp$coefficients[2])
    projection <- decomp$projection
  }
  list(coefficients = coefs, projection = projection, 
       remainder = x - projection)
}

## ----vector-decomp05-----------------------------------------------------
v1 <- c(1, 0, 0)
v2 <- c(1, 1, 1)
v3 <- c(1, 2, 3)
x  <- c(2, 7, 3)
vdecomp(x, v1, v2, v3) %>% lapply(round, digits = 8)

## ----projections01-------------------------------------------------------
project(c(1, 0), c(1, 1))
project(c(1, 0), c(1, -1))
project(c(1, 0), c(1, 2))
project(c(1, 2, 3), c(1, 1, 1))
fractions(project(c(1, 1, 1), c(1, 2, 3)))
project(c(1, 2, 3), c(1, -1, 0))
project(c(1, 2, 3, 4), c(1, 1, -1, -1))
project(c(1, 1, -1, -1), c(1, -1, 1, -1))

## ----orthonormal-s1------------------------------------------------------
x <- c(1, 1, 1)
y <- c(1, 1, -2)
w <- y - project(y, x)
dot(x, w)                                    # confirm normality
# these two column vectors are orthogonal and have correct span
cbind( x / vlength(x), w / vlength(w) )

## ----mat-dim01-----------------------------------------------------------
M = 1:12
dim(M) = c(3, 4)
M

## ----mat-dim02-----------------------------------------------------------
dim(M)

## ----matrix--------------------------------------------------------------
x <- 1:12
matrix(x, nr = 2)                  # 2 rows, entries columnwise
matrix(x, nr = 3, byrow = TRUE)    # 3 rows, entries rowwise
matrix(x, nc = 3, byrow = TRUE)    # 3 columns, entries rowwise
x                                  # x is unchanged

## ----matrix-recycle------------------------------------------------------
matrix(1, nr = 4, nc = 3)          # matrix of all 1's
matrix(nr = 3, nc = 2)             # matrix of missing data

## ----rbind-cbind---------------------------------------------------------
A = rbind(1:3, 4:6); A
B = cbind(c(5, 2, 4), c(1, 3, -1)); B

## ----as-matrix-----------------------------------------------------------
x <- 1:3
A %*% x       # vector x treated as a column matrix
as.matrix(x)  # explicit conversion to a column matrix

## ----matrix-t------------------------------------------------------------
t(1:4)                             # transpose column into row
M
t(M)                              

## ----matrix-mult---------------------------------------------------------
A %*% B            # Note: A*B does not work
B %*% A

## ----matrix-dot----------------------------------------------------------
1:4 %*% 1:4
sum(1:4 * 1:4)

## ----matrix-outer--------------------------------------------------------
outer(1:4, 1:4)

## ----matrix-outer-fun----------------------------------------------------
outer(1:4, 1:4, 
      FUN = function(x, y) {paste(x, ':', y, sep = '')})

## ----matrix-solve--------------------------------------------------------
x <- as.matrix(c(3, 1))           # vector as column matrix
A <- rbind(c(5, 2), c(3, 1))
Ainv <- solve(A); Ainv            # solve() computes inverse
A %*% Ainv
Ainv %*% A
Ainv %*% x                        # solution to system

## ----project-matrix01----------------------------------------------------
v1 <- c(1, 1, 1, 1)
v2 <- c(1, 2, -3, 0)
A <- cbind(v1 / vlength(v1), v2 / vlength(v2))

## ----project-matrix02----------------------------------------------------
t(A) %*% v1 
t(A) %*% v2
x <- 1:4
coefs <- t(A) %*% x; coefs
pr <- A %*% coefs; pr
remainder <- x - pr; remainder
dot(remainder, v1)    # should be 0
dot(remainder, v2)    # should be 0


## ----Chap1-4Review, child="Chap1-4Review.Rnw", eval=includeApp[4]--------

## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Rev-")

## ----rev-data, fig.show='hide'-------------------------------------------
names(batting)
require(Hmisc)
summary(HR~team, data=batting, fun=max,
        subset=(year==2005&league=="AL"), nmin=1)
histogram(~AB, data=batting, subset=year==2005)
xyplot(HR~H, subset=(team=="DET" & year==2005), data=batting)
bwplot(HR~league, data=batting, subset=year==2005)

## ----rev-data-fig, echo=FALSE--------------------------------------------
names(batting)
require(Hmisc)
summary(HR~team, data=batting, fun=max,
        subset=(year==2005&league=="AL"), nmin=1)
histogram(~AB, data=batting, subset=year==2005)
xyplot(HR~H, subset=(team=="DET" & year==2005), data=batting)
bwplot(HR~league, data=batting, subset=year==2005)

## ----rev-moments-binom---------------------------------------------------
x <- 0:20
sum( x * dbinom(x,20,0.25) )
sum( x^2 * dbinom(x,20,0.25) )
sum( x^2 * dbinom(x,20,0.25) ) - ( sum( x * dbinom(x,20,0.25) ) )^2

## ----rev-moments-exp-sol-------------------------------------------------
f1 <- function(x) { x * dexp(x, rate=2) }
f2 <- function(x) { x^2 * dexp(x, rate=2) }
integrate(f1, 0, Inf)
integrate(f2, 0, Inf)

## ----rev-test-coin-review------------------------------------------------
binom.test(60,100)
prop.test(60,100)

## ----rev-test-coin-power1-sol--------------------------------------------
binom.test(61,100)

## ----rev-test-coin-power2-sol--------------------------------------------
prob <- c( seq(0, 0.4, by=0.10), 0.45, 0.5, 0.55, seq(0.6, 1, by=0.10) )
power <- pbinom(39,100,prob) + 1- pbinom(60,100,prob)
print(cbind(prob,power))

## ----rev-test-coin-power3-sol--------------------------------------------
prob <- c( seq(0, 1, by=0.01) )
power <- pbinom(39,100,prob) + 1- pbinom(60,100,prob)
xyplot(power~prob, type="l", 
            main="Power to detect a biased coin with 100 flips",
            xlab="true probability of heads")

## ----rev-min-of-unif01-sol-----------------------------------------------
n <- round((1:10)^(1.75)); prob <- 1 - (0.95)^n
print(cbind(n,prob))

## ----rev-min-of-unif02-sol-----------------------------------------------
y <- c( seq(0, 1, by=0.01) )
n <- c(1,5,10,20)
n <- rep(n,times=length(y))
y <- rep(y,each=4)

density <-  n * (1-y)^{n-1}
groups <- paste("n =", n)
groups <- factor(groups, levels=unique(groups))
xyplot(density~y, groups=groups, type="l", 
            main="Pdf of the mininum of a sample from Unif(0,1)",
            key=simpleKey(levels(groups), columns=2, lines=TRUE, points=FALSE),
            xlim=c(0,0.20))

## ----rev-mix-normals01---------------------------------------------------
.3 * pnorm(12,8,2) + 0.7 * pnorm(12,16,3)

## ----rev-mix-normals02---------------------------------------------------
x <- seq(0, 30, by=0.25)
density <- 0.3 * dnorm(x,8,2) + 0.7 * dnorm(x,16,3)
xyplot(density~x, type="l", main="pdf of a mixture of normals")

## ----rev-lognormal-------------------------------------------------------
#note: these functions do not check for negative values 
#       where they shouldn't be
dlognormal <- function(x, mu=0, sigma=1) {
    dnorm(log(x), mean=mu, sd=sigma) * 1/x
}
rlognormal <- function(n, mu=0, sigma=1) {
    normals <- rnorm(n, mean=mu, sd=sigma )
    return(exp(normals))
}
plognormal <- function(x, mu=0, sigma=1) {
    pnorm( log(x), mean=mu, sd=sigma ) 
}
qlognormal <- function(p, mu=0, sigma=1) {
    exp( qnorm(p, mean=mu, sd=sigma ) )
}
# some checks
randomData <- rlognormal(100, mu=0, sigma=1/2)
quant <- quantile(randomData)
x <- qlognormal(c(0.25, 0.5, 0.75), mu=0, sigma=1/2); x
plognormal(x, mu=0, sigma=1/2)
plognormal(quant, mu=0, sigma=1/2)

plot1 <- histogram(~randomData)

x <- seq(0, 10, by=0.25)
nx <- length(x)
mu <- c(-1, 0, 1)
nmu <- length(mu)
sigma <- c(1/8, 1/4, 1/2, 1, 2, 4)
nsigma <- length(sigma)

x <- rep(x, each=nmu*nsigma)
mu <- rep(rep(mu, nsigma), times=nx)
sigma <- rep(rep(sigma, each=nmu), times=nx)
density <- dlognormal(x, mu, sigma)

xyplot(density~x|paste("sigma", '=', sigma), 
                groups = paste("mu =", mu), 
                type="l", 
                key=simpleKey(paste("mu =", sort(unique(mu))),
                        points=FALSE, lines=TRUE, columns=3),
                scales=list(y=list(relation="free", alternating=FALSE)),
                main = "pdfs of lognormal distributions")

## ----rev-faithful--------------------------------------------------------
t.test(faithful$eruptions)

## ----rev-moments-function------------------------------------------------
moment <- function(
    k=1,                                    # which moment
    vals=1:6,                               # dice by default
    probs=rep(1/length(vals), length(vals)), # uniform probs
    centered=FALSE) {                       # center on mean of data?

    if (length(k) > 1) {  # vectorize this (fancy)
        return( sapply(k, moment, vals=vals, probs=probs, centered=centered) )
    }

    if ( centered ) {
        m = sum(vals * probs)
    } else { 
        m = 0
    }
    sum((vals-m)^k * probs)
}

moment(k=1, 0:10, dbinom(0:10,10,0.4))
moment(k=2, 0:10, dbinom(0:10,10,0.4), centered=FALSE)
moment(k=2, 0:10, dbinom(0:10,10,0.4), centered=TRUE)
10 * 0.4 * 0.6   # should match previous and next value
moment(k=2, 0:10, dbinom(0:10,10,0.4), centered=FALSE) - 
    moment(k=1, 0:10, dbinom(0:10,10,0.4), centered=FALSE)^2
round(moment(k=1:4, 0:10, dbinom(0:10,10,0.4), centered=FALSE), 5)
round(moment(k=1:4, 0:10, dbinom(0:10,10,0.4), centered=TRUE), 5)

## ----rev-moments-function-cont-sol---------------------------------------
moment.cont <- function( k=1,       # which moment?
        dist = dnorm,  
        args=list(),                # arguments to dist()
        range=c(-Inf,Inf),          
        centered=FALSE) {           # centered on mean?

    if (length(k) > 1) {  # vectorize this (fancy)
        return( sapply(k, moment.cont, 
                        dist=dist, args=args,
                        range=range, centered=centered) )
    }

    if ( centered ) {
        m = moment.cont(dist=dist, range=range, k=1, centered=FALSE)
    } else { 
        m = 0
    }
    int.out <- integrate(
                    function(x) { (x-m)^k * dist(x) }, 
                    range[1], range[2])
    return (int.out$value)
}

moment.cont(dunif, k=1, centered=FALSE)
moment.cont(dunif, k=2, centered=FALSE)
moment.cont(dunif, k=2, centered=TRUE)
moment.cont(dunif, k=1:4, centered=FALSE)
round(moment.cont(dunif, k=1:4, centered=TRUE), 5)
round(moment.cont(dnorm, k=1:4, centered=TRUE), 5)
round( moment.cont(function(x) {dnorm(x, 10, 3)}, k=1:4, centered=TRUE), 5)


## ----additional-packages, include = FALSE--------------------------------
require(grid)
require(MASS)
require(car)
require(effects)
require(DAAG)
require(faraway)
require(alr3)
require(multcomp)
require(knitr)
require(corrgram)
require(latticeExtra)
require(knitr)
require(cubature)
require(vcd)

## ----session-info--------------------------------------------------------
devtools::session_info()

