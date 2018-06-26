## ----fastR-setup,include=FALSE, cache = FALSE----------------------------
includeChapter <- 1:7 %in% (1:7) # [-6]
includeApp <- 1:4 %in% 1:3

require(MASS)   # make sure this comes before dplyr loads
require(Matrix) # make sure this comes before mosaic loads
require(fastR2)
require(mosaic)
theme_set(theme_bw())
require(knitr)
require(xtable)
options(xtable.floating = FALSE)
opts_knit$set(width=74)
opts_knit$set(self.contained=FALSE)
opts_chunk$set(
  digits = 3,
  dev="pdf",  # don't need EPS files anymore
  dev.args=list(colormodel="cmyk"),
  comment="##",
  prompt=FALSE,
  size="small",
  cache=TRUE,
  cache.path='cache/c-',
  cache.lazy=FALSE,
  tidy=FALSE,
  fig.width=8*.45,
  fig.height=6*.45,
  fig.show="hold",
  fig.align="center",
  out.width=".47\\textwidth",
#  background="gray88",
#  background="white",
#  background="transparent",
  boxedLabel=TRUE
  )

opts_template$set(fig3 = list(fig.height = 7*.40, fig.width = 8*.40, out.width=".31\\textwidth"))
opts_template$set(figtall = list(fig.height = 8*.45, fig.width = 8*.45, out.width=".47\\textwidth"))
opts_template$set(fig1 = list(fig.height = 3*0.9, fig.width = 8 * 0.9, out.width=".95\\textwidth"))
opts_template$set(figbig = list(fig.height = 9*0.9, fig.width = 12*0.9, out.width=".95\\textwidth"))

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
      "\\definecolor{shadecolor}{gray}{0.8}", x, fixed = TRUE)
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


# # omit some of the output from summary( lm( ) )
# print.summary.lm <- 
#   function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
#             signif.stars = getOption("show.signif.stars"), ...) 
#   {
#     output <- capture.output( stats:::print.summary.lm(x, digits=digits, symbolic.cor = symbolic.cor,
#                               signif.stars=signif.stars, ...) )
#     l <- sapply( output, nchar )
#     w1 <- min( grep("Call", output) ) 
#     w2 <- min( grep("Resid", output) ) 
#     w3 <- min( grep("Coef", output) ) 
# 	rows <- 1:length(output)
# 	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
#     cat( paste(output[keep], collapse="\n") )
#     return(invisible(x))
#   }
# 
# print.summary.glm <- 
#   function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
#             signif.stars = getOption("show.signif.stars"), ...) 
#   {
#     output <- capture.output( stats:::print.summary.glm(x, digits=digits, symbolic.cor = symbolic.cor,
#                               signif.stars=signif.stars, ...) )
#     l <- sapply( output, nchar )
#     w1 <- min( grep("Call", output) ) 
#     w2 <- min( grep("Resid", output) ) 
#     w3 <- min( grep("Coef", output) ) 
# 	rows <- 1:length(output)
# 	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
#     cat( paste(output[keep], collapse="\n") )
#     return(invisible(x))
#   }

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

## ----amsPreface, child="amsPreface.Rnw", eval=TRUE-----------------------

## ----snippet, eval=FALSE-------------------------------------------------
## snippet("snippet")

## ----snippet2, eval=FALSE------------------------------------------------
## snippet("snippet", exec = FALSE)


## ----IntroChapter, child="IntroChapter.Rnw", eval=TRUE-------------------

## ----intro-setup, include = FALSE----------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Intro-")


## ----Data, child="Data.Rnw", eval=includeChapter[1]----------------------

## ----data-setup, include = FALSE, cache = FALSE--------------------------
knitr::opts_chunk$set(cache.path = "cache/Data-")
require(ggformula)

## ------------------------------------------------------------------------
require(fastR2)

## ------------------------------------------------------------------------
library(fastR2)

## ----data01--------------------------------------------------------------
require(fastR2)   # load fastR2 and its dependencies
glimpse(iris)     # glimpse lives in the dplyr package

## ----data02--------------------------------------------------------------
head(iris, n = 3)            # first three rows

## ----data03--------------------------------------------------------------
tail(iris, n = 3)            # last three rows

## ----data04--------------------------------------------------------------
iris[50:51, 3:5]  # 2 rows and 3 columns

## ----data05--------------------------------------------------------------
sample(iris, 6)      # this requires mosaic::sample()

## ----eval=FALSE----------------------------------------------------------
## snippet("data01")

## ----eval=FALSE----------------------------------------------------------
## snippet("data01", exec = FALSE)

## ----eval = FALSE--------------------------------------------------------
## snippet("death-penalty")

## ----iris-vector01-------------------------------------------------------
iris$Sepal.Length    # get one variable and display as vector

## ----iris-vector02-------------------------------------------------------
with(iris, Species)  # alternative method

## ----data-reload---------------------------------------------------------
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

## ----scatter01, fig.show = "hide"----------------------------------------
gf_point(Sepal.Length ~ Sepal.Width, data = iris)

## ----scatter02, fig.keep = "none"----------------------------------------
# Two ways to add facets to a scatter plot
gf_point(Sepal.Length ~ Sepal.Width | Species, data = iris)
gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
  gf_facet_wrap( ~ Species)

## ----scatter03, fig.keep = "none"----------------------------------------
gf_point(Sepal.Length ~ Sepal.Width, data = iris,
         color = ~ Species, shape = ~ Species, alpha = 0.7)

## ----scatter01-fig, echo=FALSE-------------------------------------------
gf_point(Sepal.Length ~ Sepal.Width, data = iris)

## ----scatter02-03-fig, echo=FALSE, fig.keep = c(2, 4)--------------------
# Two ways to add facets to a scatter plot
gf_point(Sepal.Length ~ Sepal.Width | Species, data = iris)
gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
  gf_facet_wrap( ~ Species)
  last_plot() %>% gf_refine(scale_x_continuous(breaks = 2:5))
gf_point(Sepal.Length ~ Sepal.Width, data = iris,
         color = ~ Species, shape = ~ Species, alpha = 0.7)
  last_plot() %>% gf_refine(scale_x_continuous(breaks = 2:5))

## ----tally01-------------------------------------------------------------
tally( ~ Species, data = iris)  # make a table of values

## ----tally02-------------------------------------------------------------
tally( ~ Sepal.Length, data = iris)  # make a table of values

## ----tally03-------------------------------------------------------------
tally( ~ (Sepal.Length > 6.0), data = iris)  

## ----tally04-------------------------------------------------------------
tally( ~ cut(Sepal.Length, breaks = 2:10), data = iris)

## ----tally05-------------------------------------------------------------
tally( ~ cut(Sepal.Length, breaks = 2:10, right = FALSE), 
       data = iris)

## ----histogram01, fig.keep = "none"--------------------------------------
gf_histogram( ~ Sepal.Length, data = iris)

## ----histogram02, fig.keep = "none"--------------------------------------
# manually selecting width of bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5)
# also selecting the boundary of the bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5, boundary = 8)
# manually selecting number of bins
gf_histogram( ~ Sepal.Length, data = iris, bins = 15)

## ----histogram02-fig, echo=FALSE, message=FALSE--------------------------
gf_histogram( ~ Sepal.Length, data = iris)
# manually selecting width of bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5)
# also selecting the boundary of the bins
gf_histogram( ~ Sepal.Length, data = iris, binwidth = 0.5, boundary = 8)
# manually selecting number of bins
gf_histogram( ~ Sepal.Length, data = iris, bins = 15)

## ----histogram03, eval=FALSE---------------------------------------------
## gf_histogram( ~ Sepal.Length, data = iris,
##     breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10),
##     color = "black", fill = "skyblue")
## gf_dhistogram( ~ Sepal.Length, data = iris,
##     breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10),
##     color = "black", fill = "skyblue")

## ----histogram04, fig.keep = "none"--------------------------------------
gf_histogram( ~ Sepal.Length | Species ~ ., data = iris,
              bins = 15)

## ----histogram03-fig, echo=FALSE-----------------------------------------
gf_histogram( ~ Sepal.Length, data = iris, 
    breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10), 
    color = "black", fill = "skyblue")
gf_dhistogram( ~ Sepal.Length, data = iris, 
    breaks = c(4, 5, 5.5, 6, 6.5, 7, 8, 10), 
    color = "black", fill = "skyblue")

## ----histogram04-fig, echo = FALSE---------------------------------------
gf_histogram( ~ Sepal.Length | Species ~ ., data = iris,
              bins = 15)

## ----histogram05, fig.keep = "none"--------------------------------------
gf_histogram( ~ Sepal.Length, data = iris, bins = 15) %>%
  gf_facet_wrap( ~ ntiles(Sepal.Width, 4, format = "interval")) 

## ----histogram06, eval=FALSE---------------------------------------------
## gf_histogram( ~ Sepal.Length | Species, bins = 15,
##               data = iris %>% filter(Species == "virginica"))

## ----histogram05-fig, echo=FALSE-----------------------------------------
gf_histogram( ~ Sepal.Length, data = iris, bins = 15) %>%
  gf_facet_wrap( ~ ntiles(Sepal.Width, 4, format = "interval")) 
gf_histogram( ~ Sepal.Length | Species, bins = 15,
              data = iris %>% filter(Species == "virginica"))

## ----freqpolygon, fig.keep = "none"--------------------------------------
gf_freqpoly( ~ Sepal.Length, color = ~ Species, data = iris, 
             binwidth = 0.5) 

## ----freqpolygon-fig, echo = FALSE---------------------------------------
gf_histogram( ~ Sepal.Length, data = iris, fill = "gray85", color = "black", bins = 20) %>%
  gf_freqpoly( ~ Sepal.Length, color = "black", size = 1, bins = 20)
gf_freqpoly( ~ Sepal.Length, color = ~ Species, data = iris, 
             binwidth = 0.5) 

## ----echo=FALSE----------------------------------------------------------
set.seed(123)
mydata <-
  data_frame(
    `symmetric`  = rnorm(5000, 75, 20),
    `positively skewed` = rgamma(5000, shape = 3, rate = 1/10),
    `negatively skewed` = 150 - `positively skewed`) %>%
  tidyr::gather("dist", "x")
gf_dhistogram( ~ x,  data = mydata, binwidth = 5) %>%
  gf_facet_wrap( ~ dist, ncol = 1) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(axis.text = element_blank(), axis.ticks = element_blank())
gf_histogram( ~ duration, data = MASS::geyser, bins = 20)

## ----faithful-histogram, eval=FALSE--------------------------------------
## gf_histogram( ~ duration, data = MASS::geyser, bins = 20)

## ----mean-median01-------------------------------------------------------
mean( ~ Sepal.Length, data = iris) 
median( ~ Sepal.Length, data = iris )

## ----mean-median02-------------------------------------------------------
    mean(Sepal.Length ~ Species, data = iris)   
  median(Sepal.Length ~ Species, data = iris)
df_stats(Sepal.Length ~ Species, data = iris, mean, median)

## ----mean-median03-------------------------------------------------------
  mean( ~ duration, data = MASS::geyser)
median( ~ duration, data = MASS::geyser)

## ----stem----------------------------------------------------------------
# stem does not understand the formula template
stem(MASS::geyser$duration)     

## ----pulse-hist-sol------------------------------------------------------
gf_histogram( ~ pulse, data = LittleSurvey)
gf_histogram( ~ pulse, data = LittleSurvey %>% filter(pulse > 30))
df_stats(~ pulse, data = LittleSurvey %>% filter(pulse > 30),  median, mean)

## ----number-prob01-sol---------------------------------------------------
gf_histogram( ~ number, data = LittleSurvey, binwidth = 1)

## ----number-prob02-sol---------------------------------------------------
t <- tally( ~ number, data = LittleSurvey); t
max(t)
t[which(t == max(t))]
t[which(t == min(t))]
tally(~ (number %% 2 == 0), data = LittleSurvey)

## ----number-prob03-sol---------------------------------------------------
# Alternative method
LittleSurvey %>%
  group_by(number) %>%
  summarise(total = n()) %>%
  filter(total == min(total))

## ----number-prob04-sol---------------------------------------------------
LittleSurvey %>%
  group_by(number) %>%
  summarise(total = n()) %>%
  filter(total == max(total))

## ----histograms-fig, echo=FALSE, opts.label="fig1"-----------------------
a <- rnorm(4000, mean = 10, sd = 2)
b <- rnorm(4000, mean = 10, sd = 5)
mydata <- data.frame(x = c(a, b), dist = rep(c("A", "B"), each = 4000))
gf_dhistogram( ~ x | dist, data = mydata,  bins = 30) %>% 
  gf_labs(x = "") 

## ----quantile01----------------------------------------------------------
quantile((1:10)^2)

## ----quantile02----------------------------------------------------------
quantile((1:10)^2, type = 5)

## ----quantile03----------------------------------------------------------
quantile((1:10)^2, type = 5, seq(0, 0.10, by = 0.005))

## ----quantile04----------------------------------------------------------
# note the different order of the arguments and 
# different output formats in these two functions.
quantile( ~ duration, data = MASS::geyser, probs = (0:10)/10)
qdata( ~ duration, (0:10)/10, data = MASS::geyser)
quantile( ~ duration, probs = (0:4)/4, data = MASS::geyser)
qdata( ~ duration, (0:4)/4, data = MASS::geyser)

## ----boxplot01, eval=FALSE-----------------------------------------------
## gf_boxplot(Sepal.Length ~ Species, data = iris)
## gf_boxplot(Sepal.Length ~ Species, data = iris) %>%
##   gf_refine(coord_flip())
## gf_boxplot(duration ~ "", data = MASS::geyser) %>%
##   gf_refine(coord_flip())

## ----boxplot01-fig, echo=FALSE-------------------------------------------
gf_boxplot(Sepal.Length ~ Species, data = iris)
gf_boxplot(Sepal.Length ~ Species, data = iris) %>% 
  gf_refine(coord_flip())
gf_boxplot(duration ~ "", data = MASS::geyser) %>%
  gf_refine(coord_flip())

## ----boxplots-match-fig, echo=FALSE--------------------------------------
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

Y <- data.frame(A = a, B = b, C = c, D = d, E = e, F = f)

X <- stack(Y)

Z <- data.frame(W = a, Z = b, V = c, Y = d, U = e, X = f)

#z$W <- y$A
#z$Z <- y$B
#z$V <- y$C
#z$Y <- y$D
#z$U <- y$E
#z$X <- y$F

Z <- stack(Z)

gf_histogram( ~ values | ind, data = X, binwidth = 0.75) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
  )

# levels(z$ind) <- rev(levels(z$ind))
Z$ind <- as.character(Z$ind)
gf_boxplot(values ~ ind, data = Z, range = 2.25, coef = 0) %>%
  gf_labs(x = "", y = "") %>%
  gf_refine(coord_flip())

## ----boxplot-match-sol---------------------------------------------------
favstats(values~ind, data = X)
favstats(values~ind, data = Z)

## ----fivenum01-----------------------------------------------------------
fivenum(1:11) 
quantile(1:11) 

## ----fivenum02-----------------------------------------------------------
fivenum(1:10) 
quantile(1:10) 

## ----boxplot-iqr-rule-sol------------------------------------------------
x <- c(1:20, 20)
quantile(x, .75)
iqr(x)
# not an "outlier"
x1 <- c(1:20, 16 + 1.5 * iqr(x))
# now it is an "outlier"
x2 <- c(1:20, 16 + 1.501 * iqr(x))
# combine into one data set and compare boxplots
Data <- data.frame(x = c(x1, x2), a = rep(c("A", "B"), each = 21))
gf_boxplot(x ~ a, data = Data)

## ----dispersion01--------------------------------------------------------
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

## ----dispersion02--------------------------------------------------------
mean(Sepal.Length ~ Species, data = iris)
var(Sepal.Length ~ Species, data = iris)
sd(Sepal.Length ~ Species, data = iris)
favstats(Sepal.Length ~ Species, data = iris)
df_stats(Sepal.Length ~ Species, data = iris, mean, var, sd)

## ----inspect-------------------------------------------------------------
inspect(iris)

## ----mad-----------------------------------------------------------------
mad(iris$Sepal.Length)

## ----mad-sol-------------------------------------------------------------
mad(iris$Sepal.Length)
median(abs(iris$Sepal.Length - median(iris$Sepal.Length))) * 1.4826

## ----pitching2005-era-sol------------------------------------------------
Pitching2 <- filter(Pitching2005, GS > 4)
favstats(ERA ~ lgID, data = Pitching2)
gf_boxplot(ERA ~ lgID, data = Pitching2) %>% gf_refine(coord_flip())
gf_histogram( ~ ERA | lgID ~ ., data = Pitching2, binwidth = 0.3)

## ----batting-ba-sol------------------------------------------------------
Batting2 <- droplevels(filter(Batting, AB >= 200))
Batting2 <- mutate(Batting2, BA = H / AB)
favstats(BA ~ league, data = Batting2)
gf_boxplot(BA ~ league, data = Batting2) %>% gf_refine(coord_flip())
gf_histogram( ~ BA | league, data = Batting2)

## ----batting-ba2-sol-----------------------------------------------------
gf_boxplot( BA ~ factor(year), color = ~ league, data = Batting2)

## ----death-penalty01-----------------------------------------------------
tally(death ~ victim, data = DeathPenalty)
tally(death ~ defendant, data = DeathPenalty)  # this line was missing in the printed version

## ----death-penalty02-----------------------------------------------------
tally(death ~ defendant | victim, data = DeathPenalty)

## ----vcd-mosaic-fig, echo=FALSE, message=FALSE---------------------------
vcd::mosaic(~ victim + defendant + death, 
            shade = TRUE,
            data = DeathPenalty %>%
              mutate(  # abbreviate labels to fit plot better
                victim = abbreviate(victim, 2),
                defendant = abbreviate(defendant, 2),
                death = abbreviate(death, 1))
)

## ----death-penalty03, fig.show="hide"------------------------------------
vcd::mosaic( ~ victim + defendant + death, data = DeathPenalty)
vcd::structable(~ victim + defendant + death, data = DeathPenalty)

## ----faithful-sol--------------------------------------------------------
gf_point(waiting ~ duration, data = MASS::geyser)
gf_point(lead(waiting) ~ duration, data = MASS::geyser)
gf_point(waiting ~ lag(duration), data = MASS::geyser)

## ----utilities-sol-------------------------------------------------------
gf_point(ccf ~ (year + month/12), data = Utilities, color = ~ factor(month)) %>%
  gf_line(ccf ~ (year + month/12), data = Utilities, color = ~ factor(month))
gf_boxplot(ccf ~ factor(month), data = Utilities)

## ----utilities-ccfpday,  eval=FALSE--------------------------------------
## Utilities <- mutate(Utilities, ccfpday = ccf / billingDays)

## ----utilities-ccfpday-sol-----------------------------------------------
Utilities <- mutate(Utilities, ccfpday = ccf / billingDays)
gf_point(ccfpday ~ (year + month/12), data = Utilities, color = ~ factor(month))
gf_boxplot(ccfpday ~ factor(month), data = Utilities)

## ----utilities-temp-sol--------------------------------------------------
gf_point(ccf ~ temp, data = Utilities)

## ----utilities-price-sol-------------------------------------------------
gf_point(ccf/gasbill ~ (12*year + month), data = Utilities)

## ----births-sol----------------------------------------------------------
gf_point(births ~ dayofyear, color = ~ wday, data = Births78)
gf_line(births ~ dayofyear, color = ~ wday, data = Births78)
gf_boxplot(births ~ wday, data = Births78)

## ----births02-sol, opts.label = "fig1"-----------------------------------
gf_line(births ~ day_of_year, color = ~wday, data = Births) %>%
  gf_facet_wrap(~year)
gf_freqpoly(~ births, color = ~wday, data = Births) %>%
  gf_facet_wrap(~year)
gf_boxplot(births ~ wday, color = ~wday, data = Births, size = 0.5) %>%
  gf_facet_wrap(~year)


## ----DiscreteDistribution, child="DiscreteDistributions.Rnw", eval=includeChapter[2]----

## ----discrete-setup, include = FALSE, cache = FALSE----------------------
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

## ----dice-SS01-sol-------------------------------------------------------
Dice %>% filter(A)

## ----dice-SS02-sol-------------------------------------------------------
Dice %>% filter(B)

## ----dice-SS03-sol-------------------------------------------------------
Dice %>% filter(C)

## ----dice-SS04-sol-------------------------------------------------------
Dice %>% filter(A & B)

## ----dice-SS05-sol-------------------------------------------------------
Dice %>% filter( B | C)

## ----dice-SS06-sol-------------------------------------------------------
Dice %>% filter( A & (B | C))

## ----coin-toss, echo=FALSE, opts.label="fig1", seed=0--------------------
coinTosses <- data.frame(
  outcome = rbinom(1000, 1, 0.5),
  toss = 1:1000) %>% 
  mutate(relFreq = cumsum(outcome) / toss)
gf_hline(yintercept = 0.5, col = "skyblue", size = 1, alpha = 0.8) %>%
  gf_line(relFreq ~ toss, data = coinTosses)  %>%
  gf_labs(title =  "Results of 1000 simulated coin tosses",
          y = "relative frequency",
          x = "number of tosses") %>%
  gf_lims(y = c(0, 1))

## ----coin-toss-hist, echo=FALSE------------------------------------------
MoreTosses <- data.frame(heads = rbinom(1000, 1000, 0.5))
gf_histogram( ~ (heads / 1000), data = MoreTosses, binwidth = 0.005) %>%
  gf_labs(title =  "Results of 1000 simulations\nof 1000 coin tosses",
          x = "proportion heads") %>%
  gf_lims(x =  c(0.44, 0.56))
LotsMoreTosses <- data.frame(heads = rbinom(1000, 10000, 0.5))
gf_histogram( ~ (heads / 10000), data = LotsMoreTosses, binwidth = 0.002) %>%
  gf_labs(title =  "Results of 1000 simulations\nof 10,000 coin tosses",
          x = "proportion heads") %>%
  gf_lims(x =  c(0.44, 0.56))

## ----print-sums-sol------------------------------------------------------
sums <- function(n){
    n <- n-3
    results <- character(0)
    for (x in 0:n) {
        for ( y in (0:(n-x)) ) {
            z <- n - x - y
            results <- 
              c(results, 
                paste(x + 1, "+", y+1, "+", z + 1, "=", x + y + z + 3))
        }
    }
    return(results)
}
length(sums(20))             # how many solutions?
sums(20)[1:10]               # first 10 solutions
sums(7)                      # smaller example

## ----choose01------------------------------------------------------------
choose(5, 2)

## ----choose02------------------------------------------------------------
4 * choose(13, 5) / choose(52, 5)

## ----full-house, tidy=FALSE----------------------------------------------
(choose(13, 1) *            # a number to have three of
 choose( 4, 3) *            # three of that number
 choose(12, 1) *            # a different number to have two of
 choose( 4, 2)) /           # two of that number
   choose(52, 5)

## ----two-pair-sol, tidy=FALSE--------------------------------------------
(choose(13, 2) *                   # two numbers (the pairs)
 choose( 4, 2) * choose(4, 2) *    # two suits for each pair
 choose(11, 1) * choose(4, 1)) /   # one more card of a different number
   choose(52, 5)

## ----three-kind-sol, tidy=FALSE------------------------------------------
(choose(13, 1) *                 # a number to have three of
 choose( 4, 3) *                 # three of that number
 choose(12, 2) *                 # two other numbers
 choose( 4, 1) * choose(4, 1)) / # one of each of those numbers
 choose(52, 5)

## ----birthday-problem-sol------------------------------------------------
# calculates prob of shared birthday
birthdayprob <- function(n) { 
        last <- 366 - n
        1 - ( prod(seq(last, 365)) / 365^n )
}

birthdayprob(10)
cbind(20:25, sapply(20:25, birthdayprob))

## ----birthday-sim-sol, seed=123------------------------------------------
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

## ----smokers01-sol-------------------------------------------------------
# a)
21.1 / ( 21.1 + 24.8)

## ----smokers02-sol-------------------------------------------------------
# b) After factoring out a constant from numerator and denominator 
#    we are left with
0.183 * 13 / ( 0.183 * 13 + .817 * 1 )
# c) After factoring out a constant from numerator and denominator 
#    we are left with
0.231 * 23 / ( 0.231 * 23 + .769 * 1 )

## ----defective-parts-sol, tidy=FALSE-------------------------------------
# part a
.20 + .27

# part b: 
# P(Wed-Thur | defective) = P(Wed-Thur and defective) / P(defective)
a <- .20 * .02 +       # Monday and defective
     .27 * .03         # Thursday and defective
b <- .25 * .015 +      # Tuesday and defective
     .28 * .01         # Wednesday and defective 
a / (a + b)

# part c: P( Wed-Thur | good ) = P( Wed-Thur and good ) / P(good)
c <- .20 * .98 +       # Monday and good
     .27 * .97         # Thursday and good
d <- .25 * .985 +      # Tuesday and good
     .28 * .99         # Wednesday and good 
c / (c + d)

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
# prob only good ones selected
choose(90, 4) / choose(100, 4)  
# prob lot is rejected
1 - choose(90, 4) / choose(100, 4)  
f <- function(x) { 1 - choose(100 - x, 4) / choose(100, 4) }
Data <- data.frame(reject=  sapply(10:100, f), defective = 10:100)
gf_line(reject ~ defective, data = Data, col = "navy", size = 1) %>%
  gf_labs(x = "number of defective parts",
          y = "probability of rejecting")

## ----acceptance-sampling-binomial-sol------------------------------------
# prob only good ones selected
dbinom(0, 4, 10/100)     
# prob lot is rejected
1 - dbinom(0, 4, 10/100) 

## ----acceptance-sampling-nbinom-sol--------------------------------------
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

## ----socks-sol, tidy=FALSE-----------------------------------------------
8 * 5 * 4 / choose(17, 3)       # 1 sock of each kind means no pairs
1 - (8 * 5 * 4 / choose(17, 3)) # so this is prob of getting a pair

# or do it this way
( choose(8, 2) * 9 + choose(5, 2) * 12 + choose(4, 2) * 13 +
  choose(8, 3) + choose(5, 3) + choose(4, 3) ) / choose(17, 3)  

## ----prob-plot, fig.keep="none"------------------------------------------
# this will be wrong for values not among 0, 1, 2, 3, or 4
f <- function(x) {
  factorial(4) / (16 * factorial(x) * factorial(4 - x))
}
f(0:4)   
sum(f(0:4))    # check to be sure the probabilities add to 1
my_data <- data.frame(probability = f(0:4), x = 0:4)
gf_point(probability ~ x, data = my_data)
gf_point(probability ~ x, data = my_data) %>%
  gf_segment(0 + probability ~ x + x, data = my_data) 
gf_point(probability ~ x, data = my_data) %>%
  gf_line(probability ~ x, data = my_data)

## ----prob-plot-fig, echo=FALSE, opts.label="fig3"------------------------
# this will be wrong for values not among 0, 1, 2, 3, or 4
f <- function(x) {
  factorial(4) / (16 * factorial(x) * factorial(4 - x))
}
f(0:4)   
sum(f(0:4))    # check to be sure the probabilities add to 1
my_data <- data.frame(probability = f(0:4), x = 0:4)
gf_point(probability ~ x, data = my_data)
gf_point(probability ~ x, data = my_data) %>%
  gf_segment(0 + probability ~ x + x, data = my_data) 
gf_point(probability ~ x, data = my_data) %>%
  gf_line(probability ~ x, data = my_data)

## ----prob-hist-cdf-fig, echo=FALSE---------------------------------------
gf_dist("binom", params = list(size = 4, prob = 0.5), kind = "hist", binwidth = 1)
gf_dist("binom", params = list(4, 0.5), kind = "cdf")

## ----binom, seed = 123---------------------------------------------------
randomData <- rbinom(n = 30, size = 4, prob = 0.5)
randomData
tally( ~ randomData)
vals <- setNames(0:4, 0:4)             # add labels for nicer displays below
dbinom(vals, size = 4, prob = 0.5)     # matches earlier example 
dbinom(vals, size = 4, prob = 0.5) * 30  # pretty close to our table above
pbinom(vals, size = 4, prob = 0.5)       # same as cumsum(dbinom(...))
qbinom(0.20, size = 20, prob = 0.5)   
pbinom(7, 20, 0.5)                    #  < 0.20
pbinom(8, 20, 0.5)                    # >= 0.20

## ----gf-dist, opts.label="fig3", fig.keep = "none"-----------------------
gf_dist("binom", params = list(size = 4, prob = 0.5))
gf_dist("binom", params = list(size = 4, prob = 0.5), kind = "cdf")
gf_dist("binom", params = list(size = 4, prob = 0.5), 
        kind = "histogram", binwidth = 1)

## ----gf-dist-fig, echo = FALSE, opts.label="fig3"------------------------
gf_dist("binom", params = list(size = 4, prob = 0.5))
gf_dist("binom", params = list(size = 4, prob = 0.5), kind = "cdf")
gf_dist("binom", params = list(size = 4, prob = 0.5), 
        kind = "histogram", binwidth = 1)

## ----freddy01------------------------------------------------------------
dbinom(20, 20, 0.8)            # probability of making all 20
1 - pbinom(14, 20, 0.8)        # probability of NOT making 14 or fewer
dbinom(16, 20, 0.8)            # probability of making exactly 16
gf_dist("binom", params = list(size = 20, prob = 0.8))

## ----nbinom01------------------------------------------------------------
1 - pnbinom(c(18, 28, 38, 48), size = 1, prob = 1/36)

## ----deMere-sol----------------------------------------------------------
1 - dbinom(0, 4, 1/6)   # P(at least one 6 in 4 tries) 
pgeom(3, 1/6)           # P(fail at most 3 times before getting a 6)
1 - dbinom(0, 24, 1/36) # P(at least one double 6 in 24 tries)
pgeom(23, 1/36)         # P(fail at most 23 times before getting double 6)

## ----amy-sol-------------------------------------------------------------
1 - pnbinom(13, 100, 0.92)

## ----Hamming-sol, tidy = FALSE-------------------------------------------
dbinom(0, 4, 0.05)             # P(all four bits received correctly
pbinom(1, 7, 0.05)             # P(>= 6 of 7 bits received correctly)
p <- seq(0, 1, by = 0.01)
DD <- data.frame(
  probability = c(dbinom(0, 4, p), pbinom(1, 7, p)),
	error.rate = c(p, p),
	method = rep(c("plain", "Hamming"), each = length(p)))
gf_line(probability ~ error.rate, data = DD, color = ~ method) %>%
  gf_labs(x = "bitwise error rate", y = "message error rate")

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
gf_point(probs ~ 0:15) %>%
  gf_labs(title = "Freddie", x = "misses", y = "probability")
#################################################################
pp <- dbinom(5, 5, 0.70)    # a) prob make 5 straight (= success)
pp                             

dgeom(1, pp)     # b) succeed with 1 miss (correct answer)
0.20 * pp        # miss first then make 5 straight (_not_ the answer)

1 - pgeom(1, pp)           # c) miss more than one shot before success
misses <- 0:15
probs <- dgeom(misses, pp)   # d)
#
gf_point(probs ~ misses) %>%
  gf_labs(title = "Frank", x = "misses", y = "probability")

## ----multiple-choice01-sol-----------------------------------------------
1 - pbinom(11, 20, 0.25)          # 11 or fewer correct fails
1 - pbinom(11, 20, 1/3)           # 11 or fewer correct fails
1 - pbinom(11, 20, 0.5 + 0.4 * 1/3 + 0.1 * 1/4)

## ----playoffs-part-------------------------------------------------------
### using binomial dist
1 - pbinom(1, 3, 0.6)             # win at least 2 of 3
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

## ----lady02--------------------------------------------------------------
binom.test(9, 10) %>% pval()     # same as pval(binom.test(9, 10))

## ----lady03--------------------------------------------------------------
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
binom.test(428, 428 + 152, p = 0.75) %>% pval()          

## ------------------------------------------------------------------------
binom.test(427, 428 + 152, p = 0.75) %>% pval()
1 - (binom.test(427, 428 + 152, p = 0.75) %>% pval())

## ------------------------------------------------------------------------
# expeted # of green
(428 + 152) * 0.75
# P(|X - 435| <= 7)
pbinom(435 + 7, 428 + 152, 0.75) - pbinom( 435 - 7 - 1, 428 + 152, 0.75)

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
## p <- seq(0, 1, by = 0.01)
## power <- 1 - (pbinom(60, 100, p) - pbinom(39, 100, p))
## gf_line(power ~ p, size = 1) %>%
##   gf_labs(x = expression(pi[a]))

## ----power04-fig, echo=FALSE---------------------------------------------
p <- seq(0, 1, by = 0.01)
power <- 1 - (pbinom(60, 100, p) - pbinom(39, 100, p))
gf_line(power ~ p, size = 1) %>%
  gf_labs(x = expression(pi[a]))

## ----eval=FALSE, include=FALSE-------------------------------------------
## Power_data <-
##   expand.grid(n = 1:2000, p = c(0.52, 0.55, 0.60)) %>%
##   mutate(
##     plab = paste("alt prob =", as.character(p)),
##     critical = qbinom(0.025, size = n, prob = p),
##     power = 1 - (pbinom(n - critical + 1, n, p) - pbinom(critical - 1, n, p))
##   )
## gf_line(power ~ n | plab, data = Power_data) %>%
##   gf_labs(y = "power", x = "number of coin tosses") %>%
##   gf_lims(y = c(0, 1.1))

## ----eval=FALSE, include = FALSE-----------------------------------------
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
## gf_line(power ~ n | plab, data = PowerData, size = 1) %>%
##   gf_labs(y = "power", x = "number of coin tosses") %>%
##   gf_lims(y = c(0, 1.1))

## ----power05, eval=FALSE-------------------------------------------------
## binom_power <- function(n, p_alt, alpha = 0.05) {
##   p_null <- 0.50
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
## gf_line(power ~ n | plab, data = PowerData, size = 1) %>%
##   gf_labs(y = "power", x = "number of coin tosses") %>%
##   gf_lims(y = c(0, 1.1))

## ----power05-fig, echo=FALSE, opts.label = "fig1"------------------------
binom_power <- function(n, p_alt, alpha = 0.05) {
  p_null <- 0.50
  critical_low <- qbinom(    alpha/2, size = n, prob = p_null) - 1
  critical_hi  <- qbinom(1 - alpha/2, size = n, prob = p_null) + 1
  pbinom(critical_low, n, p_alt) + 1 - pbinom(critical_hi - 1, n, p_alt)  
}

PowerData <- 
  expand.grid(n = seq(5, 10000, by = 5), p_alt = c(0.52, 0.55, 0.60)) %>%
  mutate( 
    power = binom_power(n, p_alt),
    plab = paste("alt prob =", as.character(p_alt))
  )
  
gf_line(power ~ n | plab, data = PowerData, size = 1) %>%
  gf_labs(y = "power", x = "number of coin tosses") %>%
  gf_lims(y = c(0, 1.1))

## ----power01-sol---------------------------------------------------------
qbinom(0.975, 200, 0.5)
qbinom(0.025, 200, 0.5)
pbinom(85:86, 200, 0.5)
1 - pbinom(114:115, 200, 0.5)

## ----power02-sol, tidy = FALSE-------------------------------------------
# define a function to calculate power for given sample size.
power <- function(size, null = 0.50, alt = 0.55, alpha = 0.05){ 
    size <- ceiling(size)   # make sure size is an integer
    leftCritical  <- -1 + qbinom(    alpha / 2, size, null)
    rightCritical <-  1 + qbinom(1 - alpha / 2, size, null)
    true_alpha <- 1 - pbinom(rightCritical - 1, size, null) + 
                      pbinom( leftCritical,     size, null)
    leftPower  <-     pbinom(leftCritical,      size, alt)
    rightPower <- 1 - pbinom(rightCritical - 1, size, alt)

    data.frame(
      n = size,
      alpha = true_alpha,
      null = null,
      alt = alt,
      power = leftPower + rightPower,
      lPower = leftPower,
      rPower = rightPower,
      lCritcal = leftCritical,
      rCritcal = rightCritical
    )
}

## ----power03-sol---------------------------------------------------------
power(c(200, 400, 5000)) 

## ----power04-sol---------------------------------------------------------
# find sample size with 90% power
uniroot(function(size){ power(size)$power - 0.90 }, c(400, 5000)) %>% 
  value()

## ----power05-sol, fig.keep = "last"--------------------------------------
gf_point(power ~ n, data = power(1000:1090)) %>%
  gf_hline(yintercept = 0.9, color = "red", alpha = 0.5)

## ----power06-sol---------------------------------------------------------
power(1000:2000) %>% 
  filter(power >= 0.90) %>%
  arrange(n) %>%
  head(10)

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

## ----binomial-power-sol00, warning = FALSE, echo = FALSE, opts.label = "fig1"----
binom_power_data <- 
  function(n, p_alt, alpha = 0.05, p_null = 0.5) {
    data_frame(
      x = 0:n,            # possible value of x
      null_prob = dbinom(x, n, p_null),  
      alt_prob  = dbinom(x, n, p_alt)
    )  %>% 
      arrange(null_prob) %>%  # sort by null probability
      mutate(
        cumsum_prob = cumsum(null_prob), # running total
        reject = 
          ifelse(cumsum_prob <= alpha, 
                 "reject", 
                 "don't reject")
      ) %>%
      arrange(x)    # sort by x values again
  }

binom_power_plot <- function(n, p_alt, alpha = 0.05, p_null = 0.5) {
  BPD <- binom_power_data(n = n, p_alt = p_alt, alpha = alpha, p_null = p_null) 
  BPDnull <- BPD %>% mutate(case = factor("null", levels = c("null", "alternative")))
  BPDalt <-  BPD %>% mutate(case = factor("alternative",  levels = c("null", "alternative")))
  true_alpha <- sum( ~ null_prob, data = BPD %>% filter(cumsum_prob <= alpha))
  power      <- sum( ~ alt_prob, data = BPD %>% filter(cumsum_prob <= alpha))
  
  gf_point(null_prob ~ x, color = ~ reject, alpha = 0.7, data = BPDnull) %>%
    gf_segment(null_prob + 0 ~ x + x, color = ~ reject, alpha = 0.7, data = BPDnull) %>%
    gf_point(alt_prob ~ x, color = ~ reject, alpha = 0.7, data = BPDalt) %>%
    gf_segment(alt_prob + 0 ~ x + x, color = ~ reject, alpha = 0.7, data = BPDalt) %>%
    gf_facet_grid( case ~ .) %>%
    gf_lims(
      x = range(c(
        n * p_null + c(-1, 1) * sqrt(n * p_null * (1-p_null)) * 4,
        n * p_alt + c(-1, 1) * sqrt(n * p_alt * (1-p_alt)) * 4)
      )
    ) %>%
    gf_refine(scale_color_manual(values = c("navy", "red"))) %>%
    gf_labs(
      caption = paste("nominal alpha = ", format(round(alpha, 3)),
                      "true alpha = ",    format(round(true_alpha, 3)), 
                      "; power = ",       format(round(power, 3))))
}

binom_power_plot(n = 50, p_null = 1/6, p_alt = 1/3)

## ----binomial-power-sol01------------------------------------------------
binom_power_data <- 
  function(n, p_alt, alpha = 0.05, p_null = 0.5) {
    data_frame(
      x = 0:n,            # possible value of x
      null_prob = dbinom(x, n, p_null),  
      alt_prob  = dbinom(x, n, p_alt)
    )  %>% 
      arrange(null_prob) %>%  # sort by null probability
      mutate(
        cumsum_prob = cumsum(null_prob), # running total
        reject = 
          ifelse(cumsum_prob <= alpha, 
                 "reject", 
                 "don't reject")
      ) %>%
      arrange(x)    # sort by x values again
  }
binom_power_data(10, 0.5) %>% head(3)

## ----binomial-power-sol02------------------------------------------------
binom_power <- function(n, p_alt, alpha = 0.05, p_null = 0.5) {
  binom_power_data(n = n, p_alt = p_alt, alpha = alpha, p_null = p_null) %>%
    filter(reject == "reject") %>%
    summarise(n = n, p_null = p_null, p_alt = p_alt, power = sum(alt_prob))
}
binom_power(n = 400, p_null = 1/3, p_alt = 1/4)
binom_power(n = 400, p_null = 1/3, p_alt = 1/4)$power

## ----binomial-power-sol03------------------------------------------------
Plot_Data <- 
  data_frame(
    n = 1:1000,
    power = sapply(n, function(n) binom_power(n = n, p_null = 1/3, p_alt = 1/4)$power)
  )

gf_line(power ~ n, data = Plot_Data)

## ----binomial-power-sol04------------------------------------------------
binom_power_plot <- function(n, p_alt, alpha = 0.05, p_null = 0.5) {
  BPD <- binom_power_data(n = n, p_alt = p_alt, alpha = alpha, p_null = p_null) 
  BPDnull <- BPD %>% mutate(case = factor("null", levels = c("null", "alternative")))
  BPDalt <-  BPD %>% mutate(case = factor("alternative",  levels = c("null", "alternative")))
  true_alpha <- sum( ~ null_prob, data = BPD %>% filter(cumsum_prob <= alpha))
  power      <- sum( ~ alt_prob, data = BPD %>% filter(cumsum_prob <= alpha))
  
  gf_point(null_prob ~ x, color = ~ reject, alpha = 0.7, data = BPDnull) %>%
    gf_segment(null_prob + 0 ~ x + x, color = ~ reject, alpha = 0.7, data = BPDnull) %>%
    gf_point(alt_prob ~ x, color = ~ reject, alpha = 0.7, data = BPDalt) %>%
    gf_segment(alt_prob + 0 ~ x + x, color = ~ reject, alpha = 0.7, data = BPDalt) %>%
    gf_facet_grid( case ~ .) %>%
    gf_lims(
      x = range(c(
        n * p_null + c(-1, 1) * sqrt(n * p_null * (1-p_null)) * 4,
        n * p_alt + c(-1, 1) * sqrt(n * p_alt * (1-p_alt)) * 4)
      )
    ) %>%
    gf_refine(scale_color_manual(values = c("navy", "red"))) %>%
    gf_labs(
      caption = paste("nominal alpha = ", format(round(alpha, 3)),
                      "true alpha = ",    format(round(true_alpha, 3)), 
                      "; power = ",       format(round(power, 3))))
}

binom_power_plot(n = 400, p_null = 1/3, p_alt = 1/4)

## ----mean-coins01--------------------------------------------------------
vals <- 0:4
probs <- c(1, 4, 6, 4, 1) / 16     # providing probabilities directly
sum(vals * probs)
sum(0:4 * dbinom(0:4, 4, 0.5))     # using the fact that X is binomial

## ----number-of-suits-sol-------------------------------------------------
p<-rep(NA, 4)
p[1] <- choose(4, 1) *  choose(13, 5) / choose(52, 5)
p[2] <- choose(4, 2) * (choose(26, 5) - (choose(13, 5) +  choose(13, 5))) / 
           choose(52, 5) 
p[4] <- choose(4, 1) *  choose(13, 2) * 13 * 13 * 13 / choose(52, 5)
p[3] <- 1 - sum(p[-3])   # sum of all probabilities must be 1
rbind(1:4, p)

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
gf_line( x * (1-x) ~ x, size = 1) %>%
  gf_labs(title = "Variance of a Bernoulli random variable",
    x = expression(pi), y = expression(Var(X)) )

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

## ----search-expval-sol---------------------------------------------------
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

## ----discrete-sum--------------------------------------------------------
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

## ----fumbles01-----------------------------------------------------------
m <- max(~week1, data = Fumbles)
tally( ~ factor(week1, levels = 0:m), data = Fumbles)
favstats( ~ week1, data = Fumbles)

## ----fumbles02, tidy = FALSE, include=FALSE------------------------------
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
    `observed pct` = 100 * `observed count` / 120,
    `model pct` = 100 * `model count` / 120
  )
Week1

## ----results = "asis", echo=FALSE----------------------------------------
print(xtable::xtable(Week1), include.rownames = FALSE)

## ----fumbles-fig, echo=FALSE, fig.keep = "last"--------------------------
gf_dhistogram( ~ week1, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
  gf_dist("pois", lambda = mean( ~ week1, data = Fumbles) )

## ----pois-hint-----------------------------------------------------------
dpois(0, 6/3)                         # 0 customers in 1/3 hour
dpois(2, 6/3)                         # 2 customers in 1/3 hour

## ----pois-sol------------------------------------------------------------
dpois(0, 6/3)                         # 0 customers in 1/3 hour
dpois(2, 6/3)                         # 2 customers in 1/3 hour
1-ppois(6, 6)                         # more than 6 in 1 hour
dpois(6, 6)                           # exactly 6 in 1 hour
ppois(5, 6)                           # less than 6 in 1 hour
ppois(30, 24) - ppois(19, 24)         # 20 to 30 customers in 4 hours

## ----hockey-goals01-sol--------------------------------------------------
1 - ppois(43, 206/506 * 89)

## ----hockey-goals02-sol--------------------------------------------------
1 - ppois(41, 206/506 * 88)

## ----hockey-goals03-sol--------------------------------------------------
max(which(dpois(1:30, 206/506 * 89) <= dpois(44, 206/506 * 89)))
ppois(28, 206/506 * 89)
ppois(28, 206/506 * 89) + 1 - ppois(43, 206/506 * 89)

## ----fumbles-23a-sol-----------------------------------------------------
m <- max( ~ week2, data = Fumbles)
tally( ~ factor(week2, levels = 0:m), data = Fumbles)
favstats( ~ week2, data = Fumbles)
m <- max( ~ week3, data = Fumbles)
tally( ~ factor(week3, levels = 0:m), data = Fumbles)
favstats( ~ week3, data = Fumbles)

## ----fumbles-23b-sol, fig.keep = "last"----------------------------------
gf_dhistogram(~ week2, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
  gf_dist("pois", lambda = mean( ~ week2, data = Fumbles))

## ----fumbles-23-sol3, fig.keep = "last"----------------------------------
gf_dhistogram(~ week3, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
  gf_dist("pois", lambda = mean( ~ week3, data = Fumbles)) 

## ----fumbles-all-sol, fig.keep="last"------------------------------------
Fumbles <- Fumbles %>% mutate(all = week1 + week2 + week3)
gf_dhistogram(~ all, data = Fumbles, binwidth = 1, alpha = 0.3) %>%
  gf_dist("pois", lambda = mean( ~ all, data = Fumbles)) %>%
  gf_labs(title = "All fumbles weeks 1-3") 

## ----fumbles-simulated-sol, fig.keep="last", opts.label = "fig1"---------
Sims <- data.frame(fumbles = rpois(120 * 8, 1.75), 
                   sample = rep(LETTERS[1:8], each = 120))
favstats( ~ fumbles | sample, data = Sims)
gf_dhistogram( ~ fumbles | sample, data = Sims, binwidth = 1) %>%
  gf_dist("pois", lambda = 1.75)

## ----youth-soccer--------------------------------------------------------
1 - phyper(3, m = 7, n = 5, k = 6) # from "girls' perspective"
phyper(2, m = 5, n = 7, k = 6)       # redone from "boys' perspective"

## ----lady-hyper----------------------------------------------------------
setNames(1 - phyper(-1:4, 5, 5, 5), paste0("x=", 0:5))

## ----lady-binom----------------------------------------------------------
setNames(1 - pbinom(2 * (0:5) - 1, 10, 0.5), paste0("x=", 2 * 0:5))

## ----power-lady-sol------------------------------------------------------
# compute rejection regions
binom.crit <- qbinom(0.95, 10, 0.5); binom.crit
hyper.crit <- qhyper(0.95, 5, 5, 5); hyper.crit
# now compute the power
1 - pbinom(binom.crit - 1, 10, 0.9)
1 - phyper(hyper.crit - 1, 5, 5, 5)

## ----fisher-twins01, digits = 4------------------------------------------
phyper(2, 17, 13, 12)
convictions <- rbind(dizygotic = c(2, 15), monozygotic = c(10, 3))
colnames(convictions) <- c("convicted", "not convicted")
convictions
fisher.test(convictions, alternative = "less") %>% pval()
fisher.test(convictions, alternative = "less")

## ----fisher-twins02------------------------------------------------------
fisher.test(convictions) %>% pval()

## ----ticket01-sol--------------------------------------------------------
d <- cbind(c(9, 13), c(14, 9)); d
fisher.test(d)
phyper(9, 23, 22, 22)

## ----ticket02-sol--------------------------------------------------------
d <- cbind(c(61, 103), c(69, 44)); d
fisher.test(d)
phyper(61, 61 + 103, 44 + 69, 61 + 69)

## ----ticket03-sol--------------------------------------------------------
or <- 9/13 / (14/9); c(or, 1/or)
or <- 61/103 / (69/44);  c(or, 1/or)

## ----fisher-twins01-alt-sol----------------------------------------------
phyper(3, 13, 17, 18)
convictions <- rbind(monozygotic = c(3, 10), dizygotic = c(15, 2)) 
colnames(convictions) <- c("not convicted", "convicted")
convictions
fisher.test(convictions, alternative = "less")

## ----first-digit, opts.label = "fig1"------------------------------------
firstDigit <- function(x) {
    trunc(x / 10^(floor(log10(abs(x)))))
}
# rivers contains lengths (mi) of 141 major North American rivers
# Rivers has first digits of lengths in miles and km
Rivers <-
  data_frame(
    digit = 1:9,
    model = log10(digit + 1) - log10(digit),
    miles = as.numeric(tally( ~ firstDigit(rivers), format = "prop")),
    km = as.numeric(tally( ~ firstDigit(1.61 * rivers), format = "prop"))
  ) %>%
  tidyr::gather(source, proportion, model:km)
    
gf_point(proportion ~ digit, color = ~ source, data = Rivers) %>%
  gf_line(proportion ~ digit, color = ~ source, data = Rivers) %>%
  gf_refine(scale_x_continuous(breaks = 1:10))

## ----multinom, fig.keep="none"-------------------------------------------
# P(X1 = 20 & X2 = 30 & X3 = 50)
dmultinom(c(20, 30, 50), size = 100, prob = c(0.2, 0.3, 0.5))
# 1 column for each of 10 random draws from Multinom(100, <0.2, 0.3, 0.5>)
rmultinom(10, size = 100, prob = c(0.2, 0.3, 0.5))
# create a data frame with 1000 draws
SimMultinom <-
  rmultinom(2000, size = 100, prob = c(0.2, 0.3, 0.5)) %>% 
  t() %>% data.frame() 
head(SimMultinom, 3)
# scatter plot shows the negative correlation between X1 and X2
gf_point(X2 ~ X1, data = SimMultinom, alpha = 0.2) %>%
  gf_density2d() %>%
  gf_labs(x = expression(X[1]), y = expression(X[2]))

## ----multinom-fig, echo = FALSE, results = "hide"------------------------
# P(X1 = 20 & X2 = 30 & X3 = 50)
dmultinom(c(20, 30, 50), size = 100, prob = c(0.2, 0.3, 0.5))
# 1 column for each of 10 random draws from Multinom(100, <0.2, 0.3, 0.5>)
rmultinom(10, size = 100, prob = c(0.2, 0.3, 0.5))
# create a data frame with 1000 draws
SimMultinom <-
  rmultinom(2000, size = 100, prob = c(0.2, 0.3, 0.5)) %>% 
  t() %>% data.frame() 
head(SimMultinom, 3)
# scatter plot shows the negative correlation between X1 and X2
gf_point(X2 ~ X1, data = SimMultinom, alpha = 0.2) %>%
  gf_density2d() %>%
  gf_labs(x = expression(X[1]), y = expression(X[2]))

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

## ----burger-barn-sol-----------------------------------------------------
prob <- (10 - (0:9)) / 10
# expected value
sum(1/prob)
# variance
sum((1 - prob) / prob^2)


## ----ContinuousDistributions, child="ContinuousDistributions.Rnw", eval=includeChapter[3]----

## ----cont-setup, include = FALSE, cache = FALSE--------------------------
knitr::opts_chunk$set(cache.path = "cache/Cont-")
require(grid)

## ----pdfdef, include=FALSE-----------------------------------------------
p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
gf_dhistogram( ~ x, bins = 20, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 40, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 161, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

## ----pdfdef-fig, echo=FALSE, opts.label="fig3"---------------------------
p <- ppoints(10000)
x <- qnorm(p, 10, 2.5) # x <- rnorm(40000, 10, 2.5)
gf_dhistogram( ~ x, bins = 20, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 40, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
gf_dhistogram( ~ x, bins = 161, color = "black", fill = "navy", alpha = 0.5) %>%
  gf_labs(x = "", y = "") %>%
  gf_theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

## ----pdf-fig, echo=FALSE, results="hide"---------------------------------
# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
gf_line(y ~ x, data = data_frame(x = seq(-1, 4, by = 0.01), y = f(x)),
        group = ~ (x > 3)) %>%
  gf_labs(y = "f(x)")

## ----pdf, fig.keep = "none"----------------------------------------------
# define the pdf for X
f <- function(x) { x^2 / 9  * (0 <= x & x <= 3) }
# numerical integration gives approximation and tolerance
integrate(f, 0, 3)
integrate(f, -Inf, Inf)              # same value but less precise
integrate(f, 0, 1)
integrate(f, 0, 1) %>% value()       # just the approximation value
# find nearby fraction
integrate(f, 0, 1) %>% value() %>% fractions() 
gf_line(y ~ x, data = data_frame(x = seq(-1, 4, by = 0.01), y = f(x)),
        group = ~ (x > 3)) %>%
  gf_labs(y = "f(x)")

## ----integrate-----------------------------------------------------------
integrate(function(x) x^2 / 9, 0, 3)

## ----dist01-sol----------------------------------------------------------
kernel <- function(x) { (x - 2) * (x + 2) * (x >= -2 & x <= 2) }
k <- 1 / value(integrate(kernel, -2, 2)); k
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

## ----unif01--------------------------------------------------------------
x <- -1:11; x
f <- function(x) { 0.1 * (0 <= x & x <= 10) }
rbind(x, f(x))   # sanity check 
# numerical integration gives approximation and error estimate
integrate(f, 7, 10)   
integrate(f, 3, 7)
integrate(f, 7, 15)

## ----unif-pdf-cdf-fig, echo=FALSE----------------------------------------

gf_line(y ~ x, 
        data = data_frame(x = seq(-0.5, 1.5, by = 0.001), y = dunif(x)),
        group = ~ (x < 0) + (x<=1) ) %>%
  gf_labs(y = "f(x)")

# gf_fun(dunif(x) ~ x, xlim = c(-0.5, 1.5), n = 1000) %>%
#   gf_labs(title = "pdf for Unif(0,1)",
#           x = "x", y = expression(f(x)))

gf_fun(punif(x) ~ x, xlim = c(-0.5, 1.5)) %>%
  gf_labs(title = "cdf for Unif(0,1)",
          x = "x", y = "F(x)")

## ----unif02--------------------------------------------------------------
runif(6, 0, 10)     # 6 random values on [0,10]
dunif(5, 0, 10)     # pdf is 1/10
punif(5, 0, 10)     # half the distribution is below 5
qunif(0.25, 0, 10)  # 1/4 of the distribution is below 2.5

## ----cdf-method01--------------------------------------------------------
g <- function(y) { 1 / (2 * sqrt(y)) * (0 <= y & y <= 1) }
integrate(g, 0, 1)

## ----cdf-method02, fig.keep = "none"-------------------------------------
fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
# gf_fun is not clever about discontinuities
gf_fun(fV(v) ~ v, xlim = c(-1, 5), n = 1000) %>%
  gf_lims(y = c(0, 1))
# we can be clever if we do things manually
gf_line(y ~ v, data = data_frame(v = seq(-1, 5, by = 0.01), y = fV(v)),
        group =  ~(v < 0) + (v <=4)) %>%
  gf_lims(y = c(0, 1))

## ----cdf-method02-fig, echo = FALSE, results = "hide"--------------------
fV <- function(v)  (0 <= v & v <= 4) * 0.25 / sqrt(abs(v)) 
integrate(fV, 0, 4)
# gf_fun is not clever about discontinuities
gf_fun(fV(v) ~ v, xlim = c(-1, 5), n = 1000) %>%
  gf_lims(y = c(0, 1))
# we can be clever if we do things manually
gf_line(y ~ v, data = data_frame(v = seq(-1, 5, by = 0.01), y = fV(v)),
        group =  ~(v < 0) + (v <=4)) %>%
  gf_lims(y = c(0, 1))

## ----exp-pdf-cdf-fig, echo=FALSE-----------------------------------------
plot_data <- 
  expand.grid(
    x = seq(0, 5, by = 0.01),
    lambda = c(0.5, 1, 1.5)
  ) %>% 
  mutate(
    density = dexp(x, rate = lambda),
    probability = pexp(x, rate = lambda)
  )
gf_line(density ~ x, color = ~ factor(lambda), group = ~lambda, data = plot_data) %>%
  gf_refine(guides(color = guide_legend(expression(lambda)))) %>%
  gf_labs(title = "Exponential pdfs")

gf_line(probability ~ x, color = ~ factor(lambda), group = ~lambda, data = plot_data) %>%
  gf_refine(guides(color = guide_legend(expression(lambda)))) %>%
  gf_labs(title = "Exponential cdfs")

## ----exp-from-unif-sol, seed = 2357--------------------------------------
U <- runif(10000) 
X <- (-10 * log(U)) %>% sort()
Y1 <- rexp(10000, rate = 1/10) %>% sort()
Y2 <- rexp(10000, rate = 1/10) %>% sort()
gf_point(Y1 ~ X) %>%
  gf_abline(slope = 1, intercept = 0)
gf_point(Y2 ~ Y1) %>%
  gf_abline(slope = 1, intercept = 0)

## ----bank----------------------------------------------------------------
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
## gf_point(run ~ time, data = PoisSim %>% filter(time <= stop),
##          shape = 1, size = 0.7, col = "black") %>%
##   gf_hline(yintercept = seq(1.5, 9.5, by = 1), color = "gray60") %>%
##   gf_vline(xintercept = seq(0, stop, by = 5), color = "gray60") %>%
##   gf_theme(panel.grid.major = element_blank(),
##            panel.grid.minor = element_blank())

## ----pois-sim-fig, echo=FALSE, opts.label="fig1", cache=FALSE, seed=123----
PoisSim <- 
  expand.grid(run = 1:10, i = 1:40) %>%
  group_by(run) %>%
  mutate(interval = rexp(40), time = cumsum(interval)) 
stop <- min(max(time ~ run, data = PoisSim))  # shortest run? 
stop <- 5 * trunc(stop / 5)                   # truncate to multiple of 5 
gf_point(run ~ time, data = PoisSim %>% filter(time <= stop), 
         shape = 1, size = 0.7, col = "black") %>%
  gf_hline(yintercept = seq(1.5, 9.5, by = 1), color = "gray60") %>%
  gf_vline(xintercept = seq(0, stop, by = 5), color = "gray60") %>%
  gf_theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())

## ----fumbles-time--------------------------------------------------------
1 - pexp(0.5, rate = 1.75)
dpois(0, 1.75 / 2)

## ----prob-cdf01, tidy = FALSE--------------------------------------------
f <- function(x, k=0) {x^k * x/2 }         # define pdf
integrate(f, lower = 0, upper = 2)         # check it is a pdf
integrate(f, k=1, lower = 0, upper = 2)    # expected value
integrate(f, k=2, lower = 0, upper = 2)    # E(X^2)

## ----prob-cdf02, tidy = FALSE--------------------------------------------
# compute the variance using E(X^2) - E(X)^2
value(integrate(f, k=2, lower = 0, upper = 2)) -
  value(integrate(f, k=1, lower = 0, upper = 2))^2    

## ----cdf01-sol-----------------------------------------------------------
g <- makeFun(3 * x^2 / 8 ~ x)
integrate(function(x) x * g(x), 0, 2)

## ----cdf02-sol, tidy = FALSE---------------------------------------------
integrate(function(x){ x^2 * g(x) }, 0, 2)
integrate(function(x){ x^2 * g(x) }, 0, 2)$value -
  (integrate(function(x){ x * g(x) }, 0, 2)$value)^2

## ----cdf03-sol, tidy = FALSE---------------------------------------------
g <- function(x) { 3 * x^2 / 8 }       # define pdf
integrate(f, lower = 0, upper = 2)     # check it is a pdf
xg <- function(x) { x * g(x) }
integrate(xg, lower = 0, upper = 2)    # expected value
xxg <- function(x) { x^2 * g(x) }
integrate(xxg, lower = 0, upper = 2)   # E(X^2)

# compute the variance using E(X^2) - E(X)^2
integrate(xxg, lower = 0, upper = 2)$value  - 
  (integrate(xg, lower = 0, upper = 2)$value)^2    

## ----traffic01-sol-------------------------------------------------------
a <- 1
f <- function(x) {1 / x^4}
k <- 1 / integrate(f, a, Inf)$value; k
f <- function(x) {k / x^4}
integrate(f, a, Inf)
integrate(f, 2, 3)
integrate(f, 2, 3) %>% value() %>% fractions()

## ----traffic02-sol-------------------------------------------------------
# find the median
g <- function(x) { integrate(f, a, x)$value - 0.5 }
uniroot(g, c(1, 10))$root

## ----traffic03-sol, tidy = FALSE-----------------------------------------
# check with a == 1
a
xf <- function(x) { x * f(x) }
Ex <- integrate(xf, a, Inf)$value;  Ex       # E(X)
xxf <- function(x) { x^2 * f(x) }
Exx <- value(integrate(xxf, a, Inf));  Exx   # E(X^2)
Exx - (Ex)^2                                 # variance
sqrt(Exx - (Ex)^2)                           # st dev

## ----normal-fig, echo=FALSE, opts.label = "fig1"-------------------------
plot_data <- 
  expand.grid(
    x = seq(-4, 4, by = 0.01),
    sigma = c(0.7, 1.0, 1.5)
  ) %>% 
  mutate(density = dnorm(x, 0, sigma))

gf_line(density ~ x, data = plot_data, color = ~ factor(sigma), group = ~sigma) %>%
  gf_refine(guides(color = guide_legend(expression(sigma))))

## ----normal01------------------------------------------------------------
# these two should return the same value:
pnorm(5, mean = 3, sd = 2)    # 5 is 1 st dev above the mean of 3
pnorm(1)                  

## ----empirical-rule------------------------------------------------------
pnorm(1:3) - pnorm(-1:-3)

## ----sat01---------------------------------------------------------------
pnorm(700, 500, 100) - pnorm(400, 500, 100)
1- pnorm(800, 500, 100)

## ----sat02---------------------------------------------------------------
pnorm(500, 422, 100)
pnorm(500, 475, 100)

## ----sat03---------------------------------------------------------------
qnorm(0.80, 500, 100)
qnorm(0.80, 500, 110)

## ----sat01-sol-----------------------------------------------------------
1 - pnorm(800, 500, 110)

## ----gamma-function-fig, echo=FALSE--------------------------------------
inputs <- seq(0.05, 5, by = 0.05)
gf_point(gamma(1:5) ~ 1:5, shape = 1) %>%
gf_line(gamma(inputs) ~ inputs) %>%
  gf_lims(y = c(0, factorial(4))) %>%
  gf_labs(x = "x", y = expression(Gamma(x)))

## ----gamma-pdfs-fig, echo=FALSE------------------------------------------
plot_data <- 
  expand.grid(
    x = seq(0, 6, by = 0.01),
    shape = 1:3,
    rate = 1:3
  ) %>% 
  mutate(
    density = dgamma(x, shape, rate),
    shape_label = paste("shape =", shape),
    rate_label = paste("rate =", rate)
    )

gf_line(density ~ x, data = plot_data %>% filter(rate == 2), 
        color = ~ factor(shape), group = ~shape) %>%
  gf_refine(guides(color = guide_legend("shape"))) %>%
  gf_labs(title = "Gamma pdfs (rate = 2)")

gf_line(density ~ x, data = plot_data %>% filter(shape == 2), 
        color = ~ factor(rate), group = ~rate) %>%
  gf_refine(guides(color = guide_legend("rate"))) %>%
  gf_labs(title = "Gamma pdfs (shape = 2)")

# gf_line(density ~ x | shape_label ~ rate_label, data = plot_data)

## ----weibull-pdfs-fig, echo=FALSE----------------------------------------
plot_data <- 
  expand.grid(
    x = seq(0, 6, by = 0.01),
    shape = 1:3,
    scale = 1:3
  ) %>% 
  mutate(
    density = dweibull(x, shape = shape, scale = scale),
    shape_label = paste("shape =", shape),
    scale_label = paste("scale =", scale)
    )

gf_line(density ~ x, data = plot_data %>% filter(scale == 2), 
        color = ~ factor(shape), group = ~shape) %>%
  gf_refine(guides(color = guide_legend("shape"))) %>%
  gf_labs(title = "Weibull pdfs (scale = 2)")

gf_line(density ~ x, data = plot_data %>% filter(shape == 2), 
        color = ~ factor(scale), group = ~scale) %>%
  gf_refine(guides(color = guide_legend("scale"))) %>%
  gf_labs(title = "Weibull pdfs (shape = 2)")

# gf_line(density ~ x | shape_label ~ scale_label, data = plot_data)

## ----middle-of-dist-sol--------------------------------------------------
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

## ----beta-pdfs-fig, echo=FALSE, opts.label = "figbig"--------------------
plot_data <- 
  expand.grid(
    x = seq(0, 1, by = 0.01),
    shape1 = c(0.8, 2, 4),
    shape2 = c(0.8, 2, 4)
  ) %>% 
  mutate(
    density = dbeta(x, shape1 = shape1, shape2 = shape2),
    shape1_label = paste("shape1 =", shape1),
    shape2_label = paste("shape2 =", shape2)
    )

# gf_line(density ~ x, data = plot_data %>% filter(shape2 == 2),
#         color = ~ factor(shape1), group = ~shape1) %>%
#   gf_refine(guides(color = guide_legend("shape1"))) %>%
#   gf_labs(title = "Beta pdfs (shape2 = 2)")
# 
# gf_line(density ~ x, data = plot_data %>% filter(shape1 == 2),
#         color = ~ factor(shape2), group = ~shape2) %>%
#   gf_refine(guides(color = guide_legend("shape2"))) %>%
#   gf_labs(title = "Beta pdfs (shape1 = 2)")

gf_line(density ~ x | shape2_label ~ shape1_label, data = plot_data)

## ----faithful-kde01, echo=FALSE------------------------------------------
times <- faithful$eruptions
gf_dhistogram( ~ times, binwidth = 0.25, alpha = 0.3) %>%
gf_dens( ~ times, size = 1, alpha = 0.9)

## ----kde01---------------------------------------------------------------
x <- c(2.2, 3.3, 5.1, 5.5, 5.7, 5.7, 6.9, 7.8, 8.4, 9.6)

## ----kde02---------------------------------------------------------------
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

## ----faithful-kde02, echo=FALSE------------------------------------------
gf_fun(K1(x) ~ x, xlim = c(-3, 3)) %>% gf_labs(y = expression(K[1])) 
gf_fun(K2(x) ~ x, xlim = c(-3, 3)) %>% gf_labs(y = expression(K[2])) 
gf_fun(K3(x) ~ x, xlim = c(-3, 3)) %>% gf_labs(y = expression(K[3])) 
gf_fun(K4(x) ~ x, xlim = c(-3, 3)) %>% gf_labs(y = expression(K[4])) 

## ----kde03---------------------------------------------------------------
kde <- function(data, kernel = K1, ...) {
  n <- length(data)
  scalingConstant <- 
    integrate(function(x){kernel(x, ...)}, -Inf, Inf) %>% value()
  function(x) {
    mat <- outer(x, data, 
                 FUN = function(x, data) {kernel(x - data, ...)})
    val <- rowSums(mat)
    val <- val / (n * scalingConstant)
    return(val)
  }
}

## ----kde04-fig, echo=FALSE-----------------------------------------------
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

## ----K5-fig, echo=FALSE--------------------------------------------------
K5 <- function(x, ...) { dnorm(x, sd = sqrt(1/6), ...) }
kdeplot(x, K2, xlim = c(1, 11), ylim = c(0, 0.32), n = 500,
    main = expression(K[2]))
kdeplot(x, K5, xlim = c(1, 11), ylim = c(0, 0.35), n = 500,
    main = expression(K[5]))

## ----faithful-kde03, fig.keep = "none"-----------------------------------
duration <- faithful$eruptions
gf_dens( ~ duration, kernel = "rectangular") %>% 
  gf_labs(title = "Rectangular kernel")
gf_dens( ~ duration, kernel = "triangular") %>% 
  gf_labs(title = "Triangular kernel")
gf_density( ~ duration) %>%
  gf_labs(title = "Normal kernel")
gf_density( ~ duration, adjust = 0.25) %>%
  gf_labs(title = "Normal kernel; adjust = 0.25")

density(duration)       # display some information about the kde

## ----faithful-kde03-fig, echo=FALSE, results = "hide"--------------------
duration <- faithful$eruptions
gf_dens( ~ duration, kernel = "rectangular") %>% 
  gf_labs(title = "Rectangular kernel")
gf_dens( ~ duration, kernel = "triangular") %>% 
  gf_labs(title = "Triangular kernel")
gf_density( ~ duration) %>%
  gf_labs(title = "Normal kernel")
gf_density( ~ duration, adjust = 0.25) %>%
  gf_labs(title = "Normal kernel; adjust = 0.25")

density(duration)       # display some information about the kde

## ----do-call00, eval = FALSE---------------------------------------------
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

## ----mise-sol------------------------------------------------------------
mise <- function(size = 20, reps = 100, dist = "norm", args = list(), ...) {
  results <- do(reps) * {
    data <- do.call(paste0("r", dist), c(list(n = size), args))
    distr <- function(x) { do.call(paste0("d", dist), c(list(x), args)) }
    d <- density(data, ...)
    data.frame(ise = ise(d, distr))
  }
  return(c(mise = mean( ~ ise, data = results)))
}

## ----mise-sims-sol, seed = 1234------------------------------------------
settings <- expand.grid( kernel = c("gaussian", "triangular",
                                  "rectangular", "epanechinikov"),
                         size = c(10, 30, 100),
                         adjust = c(1/3, 1, 3)
)
Results <- 
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
gf_line(mise ~ adj, color = ~ kernel, group = ~ kernel, alpha = 0.7,
       data = Results %>% mutate(adj = factor(round(adjust, 2)))) %>%
  gf_facet_grid(dist ~ size, scales = "free_y")       

## ----mise-beta-sol-------------------------------------------------------
  
gf_density( ~ rbeta(100, .5, .5)) %>%
gf_dist("beta", shape1 = 0.5, shape2 = 0.5, col = "gray50") %>%
  gf_labs(title = "adjust = 1") %>%
  gf_lims(y = c(0, 5), x = c(-0.5, 1.5))

gf_density( ~ rbeta(100, .5, .5), adjust = 1/3) %>%
gf_dist("beta", shape1 = 0.5, shape2 = 0.5, col = "gray50") %>%
  gf_labs(title = "adjust = 1/3") %>%
  gf_lims(y = c(0, 5), x = c(-0.5, 1.5))

gf_density( ~ rbeta(100, .5, .5), adjust = 3) %>%
gf_dist("beta", shape1 = 0.5, shape2 = 0.5, col = "gray50") %>%
  gf_labs(title = "adjust = 3") %>%
  gf_lims(y = c(0, 5), x = c(-0.5, 1.5))

gf_density( ~ rbeta(100, .5, .5), adjust = 0.1) %>%
gf_dist("beta", shape1 = 0.5, shape2 = 0.5, col = "gray50") %>%
  gf_labs(title = "adjust = 0.1") %>%
  gf_lims(y = c(0, 5), x = c(-0.5, 1.5))

## ----qq-norm01, fig.keep="none"------------------------------------------
x <- c(-0.16, 1.17, -0.43, -0.02, 1.06, 
       -1.35, 0.65, -1.12, 0.03, -1.44)
Plot_data <- data_frame(
  x.sorted = sort(x),
  p = seq(0.05, 0.95, by = 0.1),
  q = qnorm(p)
  )
Plot_data
gf_point(x.sorted ~ q, data = Plot_data)

## ----qq-norm01-fig, echo=FALSE, results = "hide"-------------------------
x <- c(-0.16, 1.17, -0.43, -0.02, 1.06, 
       -1.35, 0.65, -1.12, 0.03, -1.44)
Plot_data <- data_frame(
  x.sorted = sort(x),
  p = seq(0.05, 0.95, by = 0.1),
  q = qnorm(p)
  )
Plot_data
gf_point(x.sorted ~ q, data = Plot_data)
gf_qq( ~ x)               # generate the normal-quantile plot

## ----qq-norm02, fig.show="hide"------------------------------------------
ppoints(10)             # percentages for 10 data values
gf_qq(~x)               # generate the normal-quantile plot

## ----qq-norm03-fig, echo=FALSE, opts.label="fig1", seed=123--------------
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
gf_qq( ~ x ,data = simdata) %>%
  gf_facet_grid(factor(size) ~ factor(sample), scales = "free") %>%
  gf_theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank())

## ----qq-norm03, fig.keep = "none"----------------------------------------
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
gf_qq( ~ x ,data = simdata) %>%
  gf_facet_grid(factor(size) ~ factor(sample), scales = "free") %>%
  gf_theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank())

## ----qq-norm04, seed=123, eval = FALSE-----------------------------------
## # sample of size 40 from Binom(50, 0.4)
## x <- rbinom(40, 50, 0.4); x
## gf_qq( ~ x)

## ----qq-norm04-fig, seed=123, echo = FALSE, results = "hide"-------------
# sample of size 40 from Binom(50, 0.4)
x <- rbinom(40, 50, 0.4); x         
gf_qq( ~ x)

## ----qqline, fig.keep = "none"-------------------------------------------
gf_qq( ~ age | substance, data = HELPrct, alpha = 0.4) %>%
  gf_qqline(color = "red") %>%
  gf_qqline(color = "skyblue", tail = 0.10)

## ----qqline-fig, echo = FALSE, opts.label = "fig1"-----------------------
gf_qq( ~ age | substance, data = HELPrct, alpha = 0.4) %>%
  gf_qqline(color = "red") %>%
  gf_qqline(color = "skyblue", tail = 0.10)

## ----qq-weibull, eval=FALSE----------------------------------------------
## life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
## gf_qq( ~ life01, distribution = qweibull,
##        dparams = list(shape = 1.4, scale = 144)) %>%
##   gf_abline(slope = 1, intercept = 0, color = "gray50",
##             linetype = "dashed")

## ----qq-weibull-fig, echo = FALSE----------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
gf_qq( ~ life01, distribution = qweibull, 
       dparams = list(shape = 1.4, scale = 144)) %>%
  gf_abline(slope = 1, intercept = 0, color = "gray50", 
            linetype = "dashed")

## ----qq-exp01, fig.keep = "none"-----------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
mean(life01); 1 / mean(life01)
gf_qq( ~ life01, distribution = qexp, 
       dparams = list(1/mean(life01))) %>%
  gf_qqline(distribution = qexp, dparams = list(1/mean(life01)))

gf_qq( ~ life01, distribution = qexp) %>%
  gf_qqline(distribution = qexp)

## ----qq-exp01-fig, echo = FALSE, results = "hide"------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)
mean(life01); 1 / mean(life01)
gf_qq( ~ life01, distribution = qexp, 
       dparams = list(1/mean(life01))) %>%
  gf_qqline(distribution = qexp, dparams = list(1/mean(life01)))

gf_qq( ~ life01, distribution = qexp) %>%
  gf_qqline(distribution = qexp)

## ----qq-departures, echo=FALSE, seed=123, opts.label="fig1"--------------
dd <- rcauchy(50)
qqdata <- data.frame( 
  x = c(runif(100), 10 - rexp(100), rchisq(100, df = 2), dd, jitter(-dd)),
  dist = rep(c("A", "B", "C", "D"), each = 100) 
)
gf_qq( ~ x, data = qqdata) %>%
  gf_facet_wrap( ~ dist, scales = "free") %>%
  gf_theme(axis.text = element_blank(), axis.ticks = element_blank())

## ----jordan-sol----------------------------------------------------------
gf_qq( ~ points, data = Jordan8687)

## ----joint-integrate-----------------------------------------------------
f <- function(x) { 6 * x[1] * x[2]^2 }
cubature::adaptIntegrate(f, c(0, 0), c(1, 1))
g <- function(x) { 
  if (x[1] > x[2]) {return(0)}   # set value to 0 if X > Y
  return(f(x))                   # else return joint pdf
}
cubature::adaptIntegrate(
  g, c(0, 0), c(1, 1), tol = 0.01)  # tol controls precision

## ----ralph-claudia-sol---------------------------------------------------
pnorm(-10, 0, 25 / sqrt(3)) 

## ----mvnorm01-fig, echo = FALSE------------------------------------------
f <- Vectorize(function(x, y)  mvtnorm::dmvnorm(c(x, y)))
g <- Vectorize(function(x, y)  
  mvtnorm::dmvnorm(
    c(x, y), 
    sigma = rbind(c(1, 0.8), c(0.8, 1))
    ))

## ----mvnorm02-fig, include = FALSE---------------------------------------
plotFun(f(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE,  
        interactive = FALSE,
        par.settings = list(
          box.3d = list(col = "transparent"),
          axis.line = list(col = NA, lty = 1, lwd = 1)
        ),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, 
                      x = list(draw = FALSE),
                      y = list(draw = FALSE),
                      z = list(draw = FALSE))
)
plotFun(g(x, y) ~ x + y, x.lim = c(-3, 3), y.lim = c(-3, 3), npts = 22,
        surface = TRUE, 
        interactive = FALSE,
        par.settings = list(
          box.3d = list(col = "transparent"),
          axis.line = list(col = NA, lty = 1, lwd = 1)
        ),
        light.source = c(25, 50, 50),
        aspect = c(1, 0.5),
        zlab = "",
        screen = list(z = 20, x = -75),
        scales = list(arrows = FALSE, 
                      x = list(draw = FALSE),
                      y = list(draw = FALSE),
                      z = list(draw = FALSE))
)

## ----mvnorm03-fig, echo = FALSE------------------------------------------
plotFun(f(x, y) ~ x + y, x.lim = c(-3.5, 3.5), y.lim = c(-2, 2), npts = 150)
plotFun(g(x, y) ~ x + y, x.lim = c(-3.5, 3.5), y.lim = c(-2, 2), npts = 150)

## ----bvnorm01------------------------------------------------------------
A1 <- rbind(c(-1, 0), c(-2, -1))
A1 %*% t(A1)

## ----bvnorm02------------------------------------------------------------
A2 <- rbind(c(3/5, 4/5), c(2, 1))
A2 %*% t(A2)

## ----rho-----------------------------------------------------------------
A <- rbind(c(1, 0), c(-1, 0))
Sigma <- A %*% t(A); Sigma
det(Sigma)
rho <- Sigma[1,2] / (Sigma[1,1] * Sigma[2,2]); rho

## ----mvnorm01-sol--------------------------------------------------------
max(0, pnorm(0) - pnorm(0))
max(0, pnorm(1) - pnorm(-1))
max(0, pnorm(-1) - pnorm(-2))
max(0, pnorm(1) - pnorm(2))
max(0, pnorm(2) - pnorm(-2))

## ----mvnorm01------------------------------------------------------------
A <- rbind(c(1, 0, 0), c(1, 1, 0), c(2, 1, 1)); A
Sigma <- A %*% t(A); Sigma
mu <- c(0, 1, 2)
mvtnorm::rmvnorm(2, mean = mu, sigma = Sigma)

## ----mvnorm02------------------------------------------------------------
# find q such that Prob( X_1 <= q & X_2 <= q & X_3 <= q) = 0.5
mvtnorm::qmvnorm(0.5, mean = mu, sigma = Sigma)

## ----mvnorm03------------------------------------------------------------
# By the result above, this should be just under 0.5
mvtnorm::pmvnorm(upper = c(2, 2, 2), mean = mu, sigma = Sigma)
# Prob(all three are between -1 and 1)
mvtnorm::pmvnorm(lower = c(-1, -1, -1), upper = c(1, 1, 1), 
        mean = mu, sigma = Sigma)

## ----nonneg-definite-sol-------------------------------------------------
A <- rbind(c(sqrt(5), 0), c(-3/5 * sqrt(5), 4/5 * sqrt(5)))
A %*% t(A)

## ----mvn-marginal01------------------------------------------------------
A <- rbind(c(1, 0, 0), c(1, 1, 0), c(1, 2, 1))
# covariance matrix
Sigma<- A %*% t(A); Sigma
# marginal covariance matrix
Sigma[-3, -3]

## ----mvn-marginal02------------------------------------------------------
A12 <- A[1:2, 1:2]; A12
Sigma12 <- A12 %*% t(A12); Sigma12

## ----mvn-marginal03, digits = 2, seed = 12345----------------------------
# simulate 3 independent Norm(0,1) vars
Z1 <- rnorm(100000); Z2 <- rnorm(100000); Z3 <- rnorm(100000)
# create the X's from the Z's
X1 <- 1 + Z1; X2 <- 2 + Z1 + Z2; X3 <- 3 + Z1 + 2 * Z2 + Z3
data.frame( `E(X1)` = mean(X1), `E(X2)` = mean(X2),
            `Var(X1)` = var(X1), `Var(X2)` = var(X2),
            `Cov(X1, X2)` = cov(X1, X2), check.names = FALSE)  


## ----mvn-marginal04------------------------------------------------------
Sigma[-2, -2]

## ----mvn-marginal05------------------------------------------------------
data.frame(
  `mean(X3)` = mean(X3), `var(X3)` = var(X3),
  `cov(X1, X3)` = cov(X1, X3), check.names = FALSE)

## ----mvn-marginal06------------------------------------------------------
B <- A[c(1, 3, 2), c(1, 3, 2)]; B
# This is NOT the covariance matrix
B[-3, -3] %*% t(B[-3, -3])
# Nor is this (since it is the same as the matrix above)
A[-2, -2] %*% t(A[-2, -2])

## ----mvn-marginal07------------------------------------------------------
C <- rbind(c(1, 0), c(1, sqrt(5))); C
C %*% t(C)

## ----mvn-marginal08------------------------------------------------------
B <- rbind(c(1, 0, 0), 
           c(1, sqrt(5), 0), 
           c(1, 2/sqrt(5), 1/sqrt(5)))
B %*% t(B)

## ----mvn-conditional01---------------------------------------------------
B <- rbind(c(1, 0), c(1, 1)); B
C <- rbind(c(1,2)); C
Binv <- solve(B); Binv
C %*% Binv

## ----mvn-conditional02---------------------------------------------------
X3cond <- X3[round(X1) == 3 & round(X2) == 4]
favstats(X3cond)

## ----mvn-conditional03---------------------------------------------------
D <- rbind(c(1, 0), c(2, 1))
D %*% t(D)

## ----mvn-conditional04---------------------------------------------------
X2cond <- X2[round(X1, 1) == 3]
X3cond <- X3[round(X1, 1) == 3]
data.frame(
  `mean(X2|X1 = 3)` = mean(X2cond), `mean(X3|X1 = 3)` = mean(X3cond),
  `var(X2|X1 = 3)` = var(X2cond), `var(X3|X1 = 3)` = var(X3cond),
  `cov(X2, X3 | X1 = 3)` = cov(X2cond, X3cond), check.names = FALSE
  )

## ----mvn-conditional05---------------------------------------------------
Sigma
mu <- c(1, 2, 3); mu
# means
mu[2:3] + Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% (3 - mu[1])
# variance-covariance
Sigma[2:3, 2:3] - Sigma[2:3, 1] %*% solve(Sigma[1, 1]) %*% Sigma[1, 2:3]


## ----CLT, child="CLT.Rnw", eval=includeChapter[4]------------------------

## ----clt-setup, include = FALSE, cache = FALSE---------------------------
knitr::opts_chunk$set(cache.path = "cache/CLT-")
require(parallel)
options(`mosaic:parallelMessage` = FALSE)

## ----mom-unif01----------------------------------------------------------
x <- c(1.6, 2.8, 6.2, 8.2, 8.5, 8.7);  mean(x)

## ----mom-unif02----------------------------------------------------------
x <- c(0.2, 0.9, 1.9, 2.2, 4.7, 5.1); mean(x)

## ----mom-unif01-sol------------------------------------------------------
simulate <- function(size) {
    rdata <- runif(size)
    2 * mean(rdata) < max(rdata)
    }
mean(replicate(1000, simulate(6)))
mean(replicate(1000, simulate(12)))
mean(replicate(1000, simulate(24)))

## ----mom-exp-fig, echo = FALSE, results="hide"---------------------------
time <- c(49.0, 60.4, 8.9, 43.4, 34.8, 8.2, 13.6, 11.5, 99.4, 31.9)  
mean(time)
lambda.hat = 1 / mean(time); lambda.hat
Plot_data <- data_frame(x = seq(0,121, by = 0.5), density = dexp(x, rate = lambda.hat))

gf_dhistogram( ~ time, n = 10, binwidth = 10, alpha = 0.5) %>%
  gf_line(density ~ x, data = Plot_data)

breaks = seq(0, 11, by = 2)^2
gf_dhistogram( ~ time, n = 10, breaks = breaks, alpha = 0.5) %>%
  gf_line(density ~ x, data = Plot_data)

## ----mom-exp-------------------------------------------------------------
time <- c(49.0, 60.4, 8.9, 43.4, 34.8, 8.2, 13.6, 11.5, 99.4, 31.9)  
mean(time)
lambda.hat = 1 / mean(time); lambda.hat

## ----moment-sol----------------------------------------------------------
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

## ----mom-norm, digits = 4------------------------------------------------
x <- 
  c(57.9, 70.8, 86.3, 92.3, 94.2, 117.0, 118.4, 122.4, 125.8, 134.4)
n <- length(x)
mean(x)
sd(x)                    # NOT the method of moments estimate for sigma
sqrt(sum((x - mean(x))^2 / n))  # method of moments estimate for sigma
v <- 9/10 * var(x)       # working from var() and adjusting denominator
sqrt(v)

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

## ----mom-beta02----------------------------------------------------------
# algebraic solutions 
x.bar <- mean(x); x.bar
v <- var(x) * (length(x) - 1) / length(x); v
x.bar * (x.bar * (1 - x.bar) / v - 1)         # alpha = shape1
(1 - x.bar) * (x.bar * (1 - x.bar) / v - 1)   # beta = shape2

## ----sample-means, fig.show = "hide"-------------------------------------
# 5000 sample means of samples of size 16 from N(100, 12):
SamplingDist <- 
  do(5000) * c(sample.mean = mean(rnorm(16, 100, 12)))
mean(~ sample.mean, data = SamplingDist)
sd( ~ sample.mean, data = SamplingDist)
gf_dhistogram( ~ sample.mean, data = SamplingDist, 
               bins = 20, alpha = 0.5) %>%
  gf_vline(xintercept = 100) %>%
  gf_function(fun = dnorm, args = list(mean = 100, sd = 3))

gf_qq( ~ sample.mean, data = SamplingDist)

## ----sample-means-fig, echo = FALSE, results = "hide", fig.keep = "all"----
# 5000 sample means of samples of size 16 from N(100, 12):
SamplingDist <- 
  do(5000) * c(sample.mean = mean(rnorm(16, 100, 12)))
mean(~ sample.mean, data = SamplingDist)
sd( ~ sample.mean, data = SamplingDist)
gf_dhistogram( ~ sample.mean, data = SamplingDist, 
               bins = 20, alpha = 0.5) %>%
  gf_vline(xintercept = 100) %>%
  gf_function(fun = dnorm, args = list(mean = 100, sd = 3))

gf_qq( ~ sample.mean, data = SamplingDist)

## ----do, eval = FALSE----------------------------------------------------
## mean(rnorm(16, 100, 12))

## ----mom-beta-sim01, fig.show = "hide"-----------------------------------
Results <- do(1000) * beta.mom(rbeta(50, 2, 5))
gf_dhistogram( ~ shape1, data = Results, bins = 30) %>%
  gf_vline(xintercept = 2)
gf_dhistogram( ~ shape2, data = Results, bins = 30) %>%
  gf_vline(xintercept = 5)

## ----mom-beta-sim01-fig, echo = FALSE, cache = FALSE---------------------
Results <- do(1000) * beta.mom(rbeta(50, 2, 5))
gf_dhistogram( ~ shape1, data = Results, bins = 30) %>%
  gf_vline(xintercept = 2)
gf_dhistogram( ~ shape2, data = Results, bins = 30) %>%
  gf_vline(xintercept = 5)

## ----mom-beta-sim02, eval = FALSE----------------------------------------
## gf_point(shape2 ~ shape1, data = Results, alpha = 0.4) %>%
##   gf_abline(intercept = 0, slope = 5/2)
## 
## gf_dhistogram( ~ (shape2 / shape1), data = Results, bins = 30) %>%
##   gf_vline(xintercept = 2.5)

## ----mom-beta-sim-02-fig, echo = FALSE-----------------------------------
gf_point(shape2 ~ shape1, data = Results, alpha = 0.4) %>%
  gf_abline(intercept = 0, slope = 5/2)

gf_dhistogram( ~ (shape2 / shape1), data = Results, bins = 30) %>%
  gf_vline(xintercept = 2.5)

## ----miaa-ft-beta--------------------------------------------------------
# This gives the method of moments estimates 
# for the full data set
beta.mom(MIAA05$FTPct)

## ----miaa-ft-beta-sol----------------------------------------------------
miaa <- MIAA05
length(miaa$FTPct)
beta.mom(miaa$FTPct)
# remove players who took no shots
someshots <- miaa$FTPct[miaa$FTA >= 1]
length(someshots)
beta.mom(someshots) -> bmom1; bmom1
gf_qq( ~ someshots,  distribution = qbeta,  dparams = as.list(bmom1))
# remove players with fewer than 10 shots
tenshots <- miaa$FTPct[miaa$FTA >= 10]
length(tenshots)
beta.mom(tenshots) -> bmom2; bmom2
gf_qq( ~ tenshots, distribution = qbeta, dparams = as.list(bmom2))

## ----miaa-fg-beta-sol----------------------------------------------------
miaa <- MIAA05
length(miaa$FGPct)
beta.mom(miaa$FGPct)
# remove players who took no shots 
someshots <- miaa$FGPct[miaa$FGA >= 1]
length(someshots)
beta.mom(someshots) -> bmom1; bmom1
gf_qq( ~ someshots,  dist = function(x)qbeta(x, bmom1["shape1"], bmom1["shape2"]))
# remove players with fewer than 10 shots
tenshots <- miaa$FTPct[miaa$FGA >= 10]
length(tenshots)
beta.mom(tenshots) -> bmom2; bmom2
gf_qq( ~ tenshots, 
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

## ----srs, seed = 1234----------------------------------------------------
sample(1:30, 15)                   # 15 random numbers in 1-30
set.seed(123)
sample(1:30, 15, replace = TRUE)   # iid random sample
set.seed(123)
resample(1:30, 15)                 # same iid random sample

## ----sample-do, fig.keep = "none"----------------------------------------
sample(vcd::VonBort, 10)           # SRS of size 10
# mean of SRS
c(mean.deaths = mean( ~ deaths, data = sample(vcd::VonBort, 10)))
# mean of an iid random sample
c(mean.deaths = mean( ~ deaths, data = resample(vcd::VonBort, 10)))
# means of 3 SRSs using do()
do (3) * c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))
# means of 3 SRSs using replicate()
replicate(3, mean(~ deaths, data = sample(vcd::VonBort, 10)))
mean( ~ deaths, data = vcd::VonBort)    # mean of entire data set
gf_dhistogram( ~ mean.deaths, binwidth = 0.1, 
  data = do (2000) * 
    c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))) %>%
  gf_vline(xintercept = 0.7)

## ----sample-do-fig, echo = FALSE, results = "hide"-----------------------
sample(vcd::VonBort, 10)           # SRS of size 10
# mean of SRS
c(mean.deaths = mean( ~ deaths, data = sample(vcd::VonBort, 10)))
# mean of an iid random sample
c(mean.deaths = mean( ~ deaths, data = resample(vcd::VonBort, 10)))
# means of 3 SRSs using do()
do (3) * c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))
# means of 3 SRSs using replicate()
replicate(3, mean(~ deaths, data = sample(vcd::VonBort, 10)))
mean( ~ deaths, data = vcd::VonBort)    # mean of entire data set
gf_dhistogram( ~ mean.deaths, binwidth = 0.1, 
  data = do (2000) * 
    c(mean.deaths = mean(~ deaths, data = sample(vcd::VonBort, 10)))) %>%
  gf_vline(xintercept = 0.7)

## ----law-large-numbers, fig.keep = "none"--------------------------------
expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
  mutate(x = rexp(6 * 1000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 1, color = "red", alpha = 0.5) %>%
  gf_labs(y = "running mean", x = "") %>%
  gf_lims(y = c(0, 3)) 

## ----law-large-numbers-fig, echo = FALSE, eval = TRUE, opts.label = "fig1"----
expand.grid(run = paste("run", 1:6), rep = 1:1000) %>%
  mutate(x = rexp(6 * 1000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 1, color = "red", alpha = 0.5) %>%
  gf_labs(y = "running mean", x = "") %>%
  gf_lims(y = c(0, 3)) 

## ----lln-cauchy, fig.keep = "none"---------------------------------------
set.seed(1234)
expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
  mutate(x = rcauchy(6 * 10000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 0, color = "red", alpha = 0.5) %>%
  gf_lims(y = c(-20, 20)) %>%
  gf_labs(y = "running mean", x = "") 

## ----lln-cauchy-fig, echo = FALSE, opts.label = "fig1"-------------------
set.seed(1234)
expand.grid(run = paste("run", 1:6), rep = 1:10000) %>%
  mutate(x = rcauchy(6 * 10000)) %>%
  group_by(run) %>% arrange(rep) %>%
  mutate(runningMean = cumsum(x) / 1:length(x)) %>%
gf_line(runningMean ~ rep | run) %>%
  gf_hline(yintercept = 0, color = "red", alpha = 0.5) %>%
  gf_lims(y = c(-20, 20)) %>%
  gf_labs(y = "running mean", x = "") 

## ----lln-cauchy-seed, fig.keep="none"------------------------------------
set.seed(123)
Data <- data_frame(
  n = 1:10000,
  x =  rcauchy(10000),
  running_mean =  cumsum(x) / (1:length(x))
)
gf_line(running_mean ~ n, data = Data) %>%
  gf_labs(y = "running mean", title = "Sample from a Cauchy Distribution")

## ----clt02-sol-----------------------------------------------------------
pnorm(3, sd = 2) - pnorm(-3, sd = 2)

## ----clt-finite-samples01------------------------------------------------
x <- c(1, 6, 6, 8, 9)
mu <- sum(x * 0.2); mu                 # population mean
v <- sum(x^2 * 0.2) - mu^2; v          # population variance
pairsums <- outer(x, x, "+")           # compute 25 sums
pairmeans <- pairsums / 2

## ----clt-finite-samples02------------------------------------------------
# sampling distribution with SRS
srs.means <- as.vector(pairmeans[lower.tri(pairmeans)]); srs.means
iid.means <- as.vector(pairmeans); iid.means

srs.mean <- sum(srs.means * 0.1); srs.mean
srs.var <- sum(srs.means^2 * 0.1) - srs.mean^2; srs.var
v / 2 * (5-2) / (5-1)
sqrt(v / 2 * (5-2) / (5-1))

var(srs.means)   # N.B: This is the INCORRECT variance

## ----clt-finite-samples-sol----------------------------------------------
x <- c(1, 6, 6, 8, 9)
mu <- sum(x * 0.2); mu                 # population mean
v  <- sum(x^2 * 0.2) - mu^2; v         # population variance
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

## ----unif-12, fig.keep = "none",warning = FALSE--------------------------
sampleSums <- replicate(5000, sum(runif(12, -0.5, 0.5)))
gf_qq( ~ sampleSums)
gf_dhistogram( ~ sampleSums, bins = 25)

## ----unif-12-fig, echo = FALSE-------------------------------------------
sampleSums <- replicate(5000, sum(runif(12, -0.5, 0.5)))
gf_qq( ~ sampleSums)
gf_dhistogram( ~ sampleSums, bins = 25)

## ----beta-clt, eval = FALSE----------------------------------------------
## BetaSims <-
##   expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
##   group_by(rep, size) %>%
##   mutate(sample.mean = mean(rbeta(size, 0.5, 0.5)))
## gf_qq( ~ sample.mean | factor(size), data = BetaSims) %>%
##   gf_facet_wrap( ~ factor(size), scales = "free")
## gf_dhistogram( ~ sample.mean | factor(size), data = BetaSims, bins = 25)

## ----beta-pdf-fig, echo = FALSE, warning = FALSE-------------------------
gf_dist("beta", shape1 = 0.5, shape2 = 0.5, xlim = c(0,1)) %>%
  gf_lims(y = c(0, 5))

## ----beta-clt-fig, echo = FALSE------------------------------------------
BetaSims <-
  expand.grid(rep = 1:1000, size = c(5, 10, 20, 40)) %>%
  group_by(rep, size) %>% 
  mutate(sample.mean = mean(rbeta(size, 0.5, 0.5)))
gf_qq( ~ sample.mean | factor(size), data = BetaSims) %>%
  gf_facet_wrap( ~ factor(size), scales = "free")
gf_dhistogram( ~ sample.mean | factor(size), data = BetaSims, bins = 25) 

## ----binomial-normal-hist-fig, echo = FALSE, opts.label = "figbig"-------
Plot_data <-
  expand.grid(
    pi = c(0.5, 0.3, 0.1, 0.05),
    n = c(10, 40, 80, 800),
    p = ppoints(50)
  ) %>%
  mutate(
    label = paste("n=", n, "; pi=", pi, sep = ""),
    group = paste("pi=", pi, "; n=", n, sep = ""),
    y = qbinom(p, n, pi),
    x = qnorm(p, n * pi, sqrt(n * pi * (1-pi)))
  ) 

gf_qq( ~ y, data = Plot_data, color = ~factor(pi)) %>%
  gf_qqline(color = "gray50") %>%
  gf_facet_wrap( ~ label, scales = "free", ncol = 4) %>%
  gf_labs(
    y = expression(qbinom(p, n, pi)),
    x = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi))))
  ) %>%
  gf_theme(legend.position = "top") %>%
  gf_refine(guides(color = guide_legend(title = "pi:")))

# p <- xyplot(
#   y ~ x | paste("n=", n, sep = "") * paste("pi=", pi, sep = ""),
#   data = Plot_data,
#   scales = list(relation = "free"),
#   cex = 0.6,
#   ylab = expression(qbinom(p, n, pi)),
#   xlab = expression(qnorm(p, n * pi, sqrt(n * pi * (1-pi)))),
#   panel = function(x, y, ...){
#     panel.abline(0, 1, ...)
#     panel.xyplot(x, y, ...)
#   })
# latticeExtra::useOuterStrips(p)

## ----binomial-clt-fig, echo = FALSE--------------------------------------
gf_dist("binom", params = list(size = 20, prob = 0.1)) %>%
  gf_dist("norm", params = list(mean = 2, sd = sqrt(0.1 * 0.9 * 20)), 
          alpha = 0.4) %>%
  gf_labs(title = expression(paste("Binomial vs Normal", "(", "n=20", ", " , pi, "= 0.10", ")")))

## ----continuity-correction-----------------------------------------------
# P(55 <= X <= 65)
pbinom(65, 100, 0.6) - pbinom(54, 100, 0.6)        
# without continuity correction:
diff(pnorm(c(55, 65), 60, sqrt(100 * 0.6 * 0.4)))
# with continuity correction:
diff(pnorm(c(54.5, 65.5), 60, sqrt(100 * 0.6 * 0.4)))

## ----binomial-ztest------------------------------------------------------
# "exact" p-value
binom.test(60, 100)
# approximate p-value
z <- (0.6 - 0.5) / sqrt(0.5 * 0.5 / 100); z
2 * (1 - pnorm(z))                         

# approximate p-value with continuity correction
z <- (0.595 - 0.5) / sqrt(0.5 * 0.5 / 100); z   # 0.595 = 59.5 / 100
2 * (1 - pnorm(z))                         

# R can automate the approximate version too:
prop.test(60, 100)            # uses continuity correction by default
prop.test(60, 100, correct = FALSE)     # turn off continuity correction

## ----voter-poll-sol------------------------------------------------------
binom.test(465, 980)
 prop.test(465, 980)

## ----battery-life-sol----------------------------------------------------
1 - pnorm(160/3, mean = 50, sd = 5 / sqrt(3))

## ----z-test01, eval = FALSE----------------------------------------------
## z_test <-
##   function (x, alternative = c("two.sided", "less", "greater"),
##             mu = 0, sigma = 1, conf.level = 0.95)
##   {
##     DNAME <- deparse(substitute(x))      # record name of data coming in
##     alternative <- match.arg(alternative)      # fancy argument matching
## 
##     # your code goes here			
##   }

## ----z-test02, eval = FALSE, tidy = FALSE--------------------------------
##     Z <- "???"; names(Z) <- "z"
##     SIGMA <- sigma; names(SIGMA) <- "sigma"
##     MU <- mu; names(MU) <- "mean"
##     ESTIMATE <- "???"; names(ESTIMATE) <- "sample mean"
##     CINT <- "???"; attr(CINT, "conf.level") <- conf.level
##     PVAL <- "???"
## 		
##     structure(list(statistic = Z, parameter = SIGMA, p.value = PVAL,
##         conf.int = CINT, estimate = ESTIMATE, null.value = MU,
##         alternative = alternative, method = "Z test for a mean",
##         data.name = DNAME),
##         class = "htest")

## ----z-test-sol----------------------------------------------------------
z_test <- function (x, 
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

## ----ci-vis, eval = TRUE, fig.keep = "none", seed = 1234-----------------
# simulate 100 intervals and plot them. 
CIsim(n = 20, samples = 100, estimand = 500, 
      rdist = rnorm, args = list(mean = 500, sd = 100),
      method = zci, method.args = list(sd = 100))

## ----ci-vis-fig, echo = FALSE, message = FALSE, seed = 1234, opts.label = "fig1"----
# simulate 100 intervals and plot them. 
CIsim(n = 20, samples = 100, estimand = 500, 
      rdist = rnorm, args = list(mean = 500, sd = 100),
      method = zci, method.args = list(sd = 100))

## ----simulate-ci---------------------------------------------------------
# an example CI from a sample of size 20
zci(rnorm(20, 500, 100))
# 10,000 simulated samples each of size 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = rnorm, 
      args = list(mean = 500, sd = 100),
	    estimand = 500, method = zci, method.args = list(sd = 100))

## ----simulate-ci-unif----------------------------------------------------
mu <- 1/2; v <- 1/12           # mean and variance
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, rdist = runif, estimand = mu,
	method = zci, method.args = list(sd = sqrt(v)))

## ----simulate-ci-beta----------------------------------------------------
mu <- 0.4 / (0.4 + 0.6); mu           # mean for beta dist 
v <- (0.4 * 0.6) / ((0.4 + 0.6)^2 * (0.4 + 0.6 + 1)); v  # var for beta dist
#
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rbeta, args = list(shape1 = 0.4, shape2 = 0.6),
	    estimand = mu, method = zci, method.args = list(sd = sqrt(v)))

## ----simulate-ci-exp-----------------------------------------------------
rate <- 1/10
v <- (1 / rate)^2                              # var of exponential
mu <- 10                                       # mean of exponential
zci(rexp(20, rate), sd = sqrt(v))$conf.int     # an example CI
#
# 10,000 simulated samples of size 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, 
      rdist = rexp, args = list(rate = rate), estimand = mu, 
      method = zci, method.args = list(sd = sqrt(v)))

## ----prop-test-----------------------------------------------------------
prop.test(9458,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9457,  10000, p = .95, correct = FALSE) %>% pval()
prop.test(9458,  10000, p = .95) %>% pval()
prop.test(9457,  10000, p = .95) %>% pval()
prop.test(9456,  10000, p = .95) %>% pval()
binom.test(9457, 10000, p = .95) %>% pval()
binom.test(9456, 10000, p = .95) %>% pval()

## ----ci-99---------------------------------------------------------------
zstar <- - qnorm(0.005); zstar
se <- 2 / sqrt(25); se
zstar * se
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
df_stats(cover ~ n, data = Sims, props)

## ----one-sided-ci-sol----------------------------------------------------
zstar <- qnorm(0.95)
c(-Inf, 8 + zstar * 3 / sqrt(16))
c(8 - zstar * 3 / sqrt(16), Inf)
tstar <- qt(0.95, df = 7)
c(-Inf, 8 + tstar * 3 / sqrt(16))
c(8 - tstar * 3 / sqrt(16), Inf)

## ----myci-sol------------------------------------------------------------
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

## ----uvec01-sol,tidy = FALSE---------------------------------------------
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

## ----t-dist-fig, echo = FALSE, opts.label = "fig1"-----------------------
x <- seq(-5, 5, by = 0.05)
l <- length(x)
Plot_data <- 
  data_frame(
    pdf = c(dnorm(x), dt(x, df = 1), dt(x, df = 2), dt(x, df = 4), dt(x, df = 10)),
    distribution = rep(c(1000, 1, 2, 4, 10), each = l),
    x = rep(x, times = 5)
    )

Plot_data$distribution <- 
  factor(Plot_data$distribution,
         labels = c("df=1", "df=2", "df=4", "df=10", "normal")
  )

line.list <- list(
  lty = c(1, 1, 1, 1, 1), # lty = c(1, 2, 3, 4, 1), 
  lwd = c(2, 2, 2, 2, 2),
  col = paste("gray", c(80, 60, 40, 20, 5), sep = "")  
)

gf_line( pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_refine(scale_color_manual(values = paste("gray", c(80, 60, 40, 20, 10), sep = "")))

## ----t-test01------------------------------------------------------------
t <- (10.3 - 10)/ (0.4 / sqrt(12)); t    # test statistic
2 * pt(-abs(t), df = 11);        # p-value using t-distribution
2 * pnorm(-abs(t));              # "p-value" using normal distribution

## ----t-ci01, digits = 4--------------------------------------------------
tstar <- qt(0.975, df = 11); tstar 
10.3 + c(-1,1) * tstar * 0.4 / sqrt(12)

## ----t-test-iris---------------------------------------------------------
V <- iris %>% filter(Species == "virginica")
# for CI; p-value not interesting here -- mu = 0
t.test( ~ Sepal.Width, data = V)       
# this gives a more interesting p-value, if mu = 3 is an interesting claim
t.test( ~ Sepal.Width, data = V, mu = 3)

## ----uvec01, tidy = FALSE------------------------------------------------
x <- c(3, 4, 5, 8)
mean(x)
var(x)
u1 <- .5 * c(1,1,1,1)
u2 <-  1 / sqrt(2) * c(1,-1,0,0)
u3 <-  1 / sqrt(6) * c(1,1,-2,0)
u4 <-  1 / sqrt(12) * c(1,1,1,-3)

## ----uvec02, tidy = FALSE------------------------------------------------
ulist <- list(u1, u2, u3, u4)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
c(dot(u1, u2), dot(u1, u3), dot(u1, u4), 
  dot(u2, u3), dot(u2, u4), dot(u3, u4))

## ----uvec03, tidy = FALSE------------------------------------------------
pList <- lapply(ulist, function(u) project(x, u)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x,x))[2:4])
3 * var(x)

## ----uvec02-sol, tidy = FALSE--------------------------------------------
x <- c(3, 4, 5, 8)
mean(x)
var(x)
u1 <- .5 * c(1,1,1,1)
u2 <-  1 / sqrt(2) * c(1,-1,0,0)
u3 <-  1 / sqrt(6) * c(1,1,-2,0)
u4 <-  1 / sqrt(12) * c(1,1,1,-3)

## ----uvec03-sol, tidy = FALSE--------------------------------------------
ulist <- list(u1, u2, u3, u4)
vlength <- function(x) sqrt(dot(x,x))
sapply(ulist, vlength)
c(dot(u1, u2), dot(u1, u3), dot(u1, u4), 
  dot(u2, u3), dot(u2, u4), dot(u3, u4))

## ----uvec04-sol, tidy = FALSE--------------------------------------------
pList <- lapply(ulist, function(u) project(x, u)); pList
sapply(pList, vlength)
sum(sapply(pList, function(x) dot(x,x))[2:4])
3 * var(x)

## ----uvec05-sol,tidy = FALSE---------------------------------------------
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

## ----uvec06-sol,tidy = FALSE---------------------------------------------
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

## ----sepal-width-ci------------------------------------------------------
iris %>% group_by(Species) %>%
  do(data.frame(as.list( 
	 confint(t.test( ~ Sepal.Width, data= .))
  )))

## ----sepal-length-ci-sol-------------------------------------------------
iris %>% group_by(Species) %>%
  do(data.frame(as.list( 
	 confint(t.test( ~ Sepal.Length, data= .))
  )))

## ----sepal-ratio-ci-sol--------------------------------------------------
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

## ----shoe-size-ci--------------------------------------------------------
qt(0.975, df = 255) * 2 / sqrt(256)

## ----uniroot-------------------------------------------------------------
f <- function(n) { qt(0.975, n-1) * 2 / sqrt(n) - 0.25}
uniroot(f, c(10, 1000))

## ----t-simulations-------------------------------------------------------
# an example CI from a sample of size 20
confint(t.test(rnorm(20, 500, 100)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 500, 
	rdist = rnorm, args = list(mean = 500, sd = 100))

## ----heavy-tails, echo = FALSE, warning = FALSE--------------------------
x <- seq(-7, 10, by = 0.10)
l <- length(x)
Plot_data <- data.frame(
    pdf = c(dnorm(x, 0, sqrt(3)), dt(x, df = 3)),
    cdf = c(pnorm(x, 0, sqrt(3)), pt(x, df = 3)),
    distribution = rep(c(1000, 3), each = l),
    x = rep(x, times = 2)
    )

Plot_data$distribution <- factor(Plot_data$distribution,
    labels = c("T(3)", "Norm")
    )

# line.list <- list(
#             lty = c(1, 1),
#             lwd = c(1.5, 1.5),
#             col = trellis.par.get("superpose.line")$col[1:2]
#             )

gf_line(pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "PDFs") %>% 
  gf_lims(x = c(0, 7), y = c(0, 0.40)) %>%
  gf_theme(legend.position = "top")

gf_line(cdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "CDFs") %>% 
  gf_lims(x = c(-2, 7), y = c(0, 1)) %>%
  gf_theme(legend.position = "top")

gf_line(pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "PDFs (zoomed)") %>% 
  gf_lims(x = c(3, 9), y = c(0, 0.06)) %>%
  gf_theme(legend.position = "top")

gf_line(cdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "CDFs (zoomed)") %>% 
  gf_lims(x = c(2.8, 9), y = c(0.97, 1)) %>%
  gf_theme(legend.position = "top")

# xyplot(pdf ~ x, Plot_data,
#     groups = distribution,
#     main = "PDFs",
#     type = "l",
#     xlim = c(0, 7),
#     ylim = c(-0.005, 0.40),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(cdf ~ x, Plot_data,
#     groups = distribution,
#     main = "CDFs",
#     type = "l",
#     xlim = c(-3, 7),
#     ylim = c(0, 1),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(pdf ~ x, Plot_data,
#     groups = distribution,
#     main = "PDFs",
#     type = "l",
#     xlim = c(3, 9),
#     ylim = c(-0.005, 0.06),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(cdf ~ x, Plot_data,
#     groups = distribution,
#     main = "CDFs",
#     type = "l",
#     xlim = c(3, 9),
#     ylim = c(0.98, 1),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )

## ----t-robust-heavytails-------------------------------------------------
# an example CI (n = 20, mu = 0)
confint(t.test(rt(20, 3)))
# 10,000 simulated samples of sizes 2, 5, and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 0, 
      rdist = rt, args = list(df = 3))

## ----t-robust-exp--------------------------------------------------------
# an example CI (n = 20; mu = 10)
confint(t.test(rexp(20, 1/10)))
# 10,000 simulated samples of sizes 2, 5 and 20
CIsim(n = c(2, 5, 20), samples = 10000, estimand = 10, 
      rdist = rexp, args = list(rate = 1/10))

## ----t-robust-qq, eval = FALSE-------------------------------------------
## ExpSims <-
##   expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rexp(n), mu = 1)),
##     dist = paste0("Exp(1); n=", n))
## 
## TSims <-
##   expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
##   group_by(n, rep) %>%
##   mutate(
##     pval = pval(t.test(rt(n, df = 3), mu = 0)),
##     dist = paste0("t(3); n=", n))
## 
## gf_qq( ~ pval, data = bind_rows(ExpSims, TSims),
##        distribution = qunif, geom = "line") %>%
##   gf_abline(slope = 1, intercept = 0, color = "red",
##             linetype = "dashed", alpha = 0.6) %>%
##   gf_facet_wrap( ~ dist, nrow = 2)
## gf_qq( ~ pval, data = bind_rows(ExpSims, TSims), na.rm = TRUE,
##        distribution = qunif, geom = "line") %>%
##   gf_abline(slope = 1, intercept = 0, color = "red",
##             linetype = "dashed", alpha = 0.6) %>%
##   gf_lims(x = c(0, 0.2), y = c(0, 0.2)) %>%
##   gf_facet_wrap( ~ dist, nrow = 2)

## ----t-robust-qq-fig, echo = FALSE, opts.label = "fig1"------------------
ExpSims <-
  expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rexp(n), mu = 1)), 
    dist = paste0("Exp(1); n=", n))

TSims <-
  expand.grid(n = c(10, 20, 40, 80), rep = 1:2000) %>%
  group_by(n, rep) %>%
  mutate(
    pval = pval(t.test(rt(n, df = 3), mu = 0)), 
    dist = paste0("t(3); n=", n))

gf_qq( ~ pval, data = bind_rows(ExpSims, TSims), 
       distribution = qunif, geom = "line") %>%
  gf_abline(slope = 1, intercept = 0, color = "red", 
            linetype = "dashed", alpha = 0.6) %>% 
  gf_facet_wrap( ~ dist, nrow = 2)
gf_qq( ~ pval, data = bind_rows(ExpSims, TSims), na.rm = TRUE,
       distribution = qunif, geom = "line") %>% 
  gf_abline(slope = 1, intercept = 0, color = "red", 
            linetype = "dashed", alpha = 0.6) %>%
  gf_lims(x = c(0, 0.2), y = c(0, 0.2)) %>%
  gf_facet_wrap( ~ dist, nrow = 2)

## ----miaa-sol------------------------------------------------------------
set.seed(12345)
Intervals <- 
  do(20) * 
  confint(t.test( ~ PTSG, data = sample(MIAA05, 15), conf.level = .90))
mu <- mean( ~ PTSG, data = MIAA05)
tally( ~ (lower <= mu & mu <= upper), data = Intervals)

## ----wald-sims-----------------------------------------------------------
WaldSims <- 
  CIsim(2000, n = c(5, 10, 20, 40), 
        method = binom.test, method.args = list(ci.method = "Wald"),
        rdist = rbinom, args = list(size = 1, prob = 0.2),
        estimand = 0.2)

## ----wald-score-compare-fig, echo = FALSE, opts.label = "fig1"-----------
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

trellis.par.set(theme = col.fastR(bw = TRUE))
if(FALSE) {
matplot(seq(0.001, 0.999, by = 0.001), tmp[, 1:3], type = "l",
    lty = 1,
    col = trellis.par.get("superpose.line")$col[1:3],
    main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
    xlab = expression(pi),
    ylab = "Coverage Rate",
    lwd = 2,
    ylim = c(0.8, 1))
    abline(h = 0.95)
    legend(0.35, 0.875, c("Score", "Wald", "Clopper-Pearson")[c(3, 1, 2)], 
        col = trellis.par.get("superpose.line")$col[c(3, 1, 2)],
        lwd = 2,
        lty = 1,
        cex = 1)

trellis.par.set(theme = col.fastR(bw = TRUE))
matplot(seq(0.001, 0.999, by = 0.001), tmp[, c(1, 8)], type = "l",
    lty = 1, col = trellis.par.get("superpose.line")$col[1:4],
    main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
    xlab = expression(pi),
    ylab = "Coverage Rate",
    lwd = 2,
    ylim = c(0.8, 1))
    abline(h = 0.95)
    legend(0.40, 0.875, c("Score", "Wilson"), col = trellis.par.get("superpose.line")$col[1:2], lty = 1, cex = 1)
}


gf_line(coverage ~ pi, data = coverage, color = ~ method,
        na.rm = TRUE, alpha = 0.8) %>%
  gf_hline(yintercept = 0.95, alpha = 0.5, linetype = "dashed") %>%
  gf_lims(y = c(0.8, 1)) %>% 
  gf_theme(legend.position = "top") %>%
  gf_labs(title = "Coverage rates (n = 35; 95% CI)") 
  
# xyplot(coverage ~ pi, data = coverage, groups = method,
# 	lty = 1, lwd = 2, alpha = 0.8,
# 	type = "l", cex = .25,
#     main = paste("Coverage rates (n=", n, "; 95% CI)", sep = ""),
#     xlab = expression(pi),
#     ylab = "Coverage Rate",
#     ylim = c(0.8, 1),
# 	# col = c("gray50", "gray80", "gray20"),
# 	col = c("navy", "red", "forestgreen"), #  "purple"),
# 	auto.key = TRUE,
# 	legend = list(
# 		inside= list(x = .5, y = .1, corner = c(.5, 0), 
# 			fun = draw.key,
# 			args = list(
# 				key = list(
# 					lines = list(lty = 1, lwd = 2,
# 						# col = c("gray70", "gray20", "gray50")
# 	          col = c("red", "forestgreen", "navy") 
# 					),
# 					text = list(
# 						lab = c("Clopper-Pearson", "Score", "Wald"),
# 						cex = .8)
# 				)
# 			)
# 		)
# 	),
# 	panel = function(x, y, ...){
# 		panel.abline(h = 0.95)
# 		panel.xyplot(x, y, ...)
# 		}
# 	)
#write.csv(coverage, file = "CIcoverage.csv", row.names = FALSE)

## ----binom-cis-----------------------------------------------------------
binom.test(25, 70)  # Clopper-Pearson
binom.test(25, 70, ci.method = "Wald")
binom.test(25, 70, ci.method = "score")
 prop.test(25, 70)  # also uses inverted score test

## ----prop-ci-sim---------------------------------------------------------
Sims <-
  expand.grid(
  n = c(35, 100), 
  pi = c(0.2, 0.3, 0.4, 0.5),
  method = c("Wald", "Wilson", "score"), 
  rep = 1:2000) %>%
  group_by(n, pi, method, rep) %>% 
  do(
    confint(
      binom.test(rbinom(1, .$n, .$pi), n = .$n, ci.method = .$method)))

Sims %>%
  group_by(n, pi, method) %>% 
  summarise(cover = prop(lower <= pi & pi <= upper))

## ----helium-footballs01--------------------------------------------------
Footballs <- HeliumFootballs %>% mutate(diff = helium - air)
Footballs %>% head(3)
t.test( ~ diff, data = Footballs)

## ----helium-footballs02--------------------------------------------------
tally( ~ sign(helium - air), data = HeliumFootballs)

## ----helium-footballs03--------------------------------------------------
binom.test(20,37) %>% pval()

## ----sign-ties-----------------------------------------------------------
S = (20 - 17) / sqrt(20 + 17); S
2 * pnorm(-S)

## ----goldfish01----------------------------------------------------------
S <- (11 - 4) / sqrt(11 + 4); S
2 * pnorm(-S)

## ----goldfish02----------------------------------------------------------
binom.test(11, 11 + 4) %>% pval()

## ----goldfish03----------------------------------------------------------
2 * pnorm(-(10 - 3) / sqrt(10 + 3))
binom.test(10, 13) %>% pval()

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
gf_line(pvalS ~ pvalU, data = Sims)

# The p-values exhibit the expected Unif(0,1) distribution
gf_dhistogram( ~ pvalU | paste0("n=", n), data = Sims, binwidth = 0.05) %>%
  gf_lims(x = c(0,1))
gf_qq( ~ pvalU | paste0("n=", n), data = Sims, distribution = qunif)

# S, U approx Normal when sample size is large enough.
gf_dhistogram( ~ U | paste0("n=", n), data = Sims, bins = 25)
gf_dhistogram( ~ S | paste0("n=", n), data = Sims, bins = 25)

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

## ----endurance-paired01--------------------------------------------------
t.test( ~ (vitamin - placebo), data = Endurance)

## ----endurance-paired02--------------------------------------------------
t.test( ~ (log(vitamin) - log(placebo)) , data = Endurance)

## ----endurance-paired03--------------------------------------------------
# this is the same as the previous one
t.test( ~ (log(vitamin / placebo)) , data = Endurance)  

## ----endurance-paired04--------------------------------------------------
t.test( ~ (vitamin / placebo), data = Endurance)

## ----endurance-paired05--------------------------------------------------
t.test(~ (1 / vitamin - 1 / placebo), data = Endurance)

## ----endurance-paired06--------------------------------------------------
binom.test(~ (vitamin > placebo), data = Endurance)

## ----Joes-coin01-sol-----------------------------------------------------
wald.ci <- function(x, n, level = 0.95) {
    alpha = 1 - level
    pi.hat <- x / n
    se <- sqrt(pi.hat * (1 - pi.hat) / n)
    z.star <- qnorm(1 - alpha / 2)
    pi.hat + c(-1, 1) * z.star * se
}

## ----Joes-coin02-sol-----------------------------------------------------
wilson.ci<- function(x, n, level = 0.95) {
    x = x + 2; n = n + 4
    alpha = 1 - level
    pi.hat <- x / n
    se <- sqrt(pi.hat * (1 - pi.hat) / n)
    z.star <- qnorm(1 - alpha / 2)
    pi.hat + c(-1, 1) * z.star * se
}

## ----Joes-coin03-sol-----------------------------------------------------
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

## ----Joes-coin04-sol-----------------------------------------------------
prop.test(115, 200)$conf.int
confint(prop.test(115, 200, correct = FALSE))

## ----Joes-coin05-sol-----------------------------------------------------
wald.ci(115, 200)
wilson.ci(115, 200)
wald.ci(117, 204)
score.ci(115, 200)

## ----Joes-coin06-sol-----------------------------------------------------
# score interval using uniroot:
p.hat <- 115 / 200; n <- 200
f <- function(p) {
    abs(p.hat - p) / sqrt(p * (1-p) / n) + qnorm(0.025)
}
uniroot(f, c(0, p.hat))$root
uniroot(f, c(p.hat, 1))$root
uniroot(f, c(0, p.hat))$estim.prec

## ----corn-sol------------------------------------------------------------
t.test( ~ (kiln - reg), data = Corn)

## ----golfballs-range, fig.keep = "none"----------------------------------
stat <- function(x) { diff(range(x)) }  
statTally(
  golfballs, rgolfballs, stat, xlab = "test statistic (range)") 

## ----golfballs-range-fig, echo = FALSE, message = FALSE, results = "hide"----
stat <- function(x) { diff(range(x)) }  
statTally(
  golfballs, rgolfballs, stat, xlab = "test statistic (range)") 

## ----golfballs-sample----------------------------------------------------
rmultinom(1, prob = c(.3, .3, .2, .2), size = 486)
tally(resample(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4), 486))

## ----fisher-twins-perm01, seed = 123-------------------------------------
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

## ----fisher-twins-perm02-------------------------------------------------
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

## ----iris-perm, fig.keep = "none"----------------------------------------
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
gf_dhistogram( ~ simStat, data = SetosaSims) %>%
  gf_vline(xintercept = testStat)
# 1-sided p-value
prop1( ~ (simStat >= testStat), data = SetosaSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = SetosaSims)

## ----iris-perm-fig, echo = FALSE, results = "hide"-----------------------
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
gf_dhistogram( ~ simStat, data = SetosaSims) %>%
  gf_vline(xintercept = testStat)
# 1-sided p-value
prop1( ~ (simStat >= testStat), data = SetosaSims)

# 2-sided p-value
2 * prop1( ~ (simStat >= testStat), data = SetosaSims)

## ----iris-perm-versi-----------------------------------------------------
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

## ----pvals01, fig.keep = "none"------------------------------------------
Pvals.null <- do(10000) * { t.test(rnorm(25, 0, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.null, binwidth = 0.02, center = 0.01)
gf_qq(~ p.value, data = Pvals.null, distribution = qunif, geom = "line")

## ----pvals01-fig, echo = FALSE, results = "hide"-------------------------
Pvals.null <- do(10000) * { t.test(rnorm(25, 0, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.null, binwidth = 0.02, center = 0.01)
gf_qq(~ p.value, data = Pvals.null, distribution = qunif, geom = "line")

## ----pvals02, fig.keep = "none"------------------------------------------
Pvals.alt <- do(10000) * { t.test(rnorm(25, 1/2, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.alt, binwidth = 0.01, center = 0.005)
gf_qq(~ p.value, data = Pvals.alt, distribution = qunif, geom = "line")

## ----pvals02-fig, echo = FALSE, results = "hide"-------------------------
Pvals.alt <- do(10000) * { t.test(rnorm(25, 1/2, 1)) %>% pval() }
gf_dhistogram(~ p.value, data = Pvals.alt, binwidth = 0.01, center = 0.005)
gf_qq(~ p.value, data = Pvals.alt, distribution = qunif, geom = "line")

## ----pvals03-------------------------------------------------------------
# power 
prop(~ (p.value <= 0.05), data = Pvals.alt)

## ----pvals04, echo = FALSE, results = "hide", fig.keep = 2, opts.label = "figtall"----
update <- function(theta, alpha, power) {
  theta * power  / (theta * power + alpha * (1-theta))
}
# gf_function(fun = update, args = list(alpha = 0.05, power = 0.66), xlim = c(0,1)) %>%
#   gf_labs(
#     y = expression(paste("updated probability that ", H[0], " is false")),
#     x = expression(paste("prior probability that ", H[0], " is false (", theta, ")"))
#   )
plotFun(update(theta, alpha = 0.05, power = 0.66) ~ theta,
        theta.lim = c(0,1),
        ylab = expression(paste("updated probability that ", H[0], " is false")),
        xlab = expression(paste("prior probability that ", H[0], " is false (", theta, ")")))
plotFun(update(theta, alpha = 0.05, power = 0.90) ~ theta,
        add = TRUE, col = "red")
plotFun(update(theta = 0.1, alpha, power = 0.66) ~ alpha,
        alpha.lim = c(0, 0.10),
        ylab = expression(paste("updated probability that ", H[0], " is false")),
        xlab = expression(paste("significance level (", alpha, ")")))
plotFun(update(theta = 0.1, alpha, power = 0.90) ~ alpha,
        add = TRUE, col = "red")

## ----dimes-boot01, digits = 4--------------------------------------------
x.bar <- mean( ~ mass, data = Dimes); x.bar 

## ----dimes-boot02, fig.keep = "none"-------------------------------------
Dimes.boot <- 
  do(5000) * c(boot.mean = mean( ~ mass, data = resample(Dimes)))
gf_dhistogram( ~ boot.mean, data = Dimes.boot)

## ----dimes-boot03, fig.keep = "none"-------------------------------------
# normality check
gf_qq( ~ boot.mean, data = Dimes.boot)
SE <- sd( ~ boot.mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

## ----dimes-boot02-fig, echo = FALSE, results = "hide"--------------------
Dimes.boot <- 
  do(5000) * c(boot.mean = mean( ~ mass, data = resample(Dimes)))
gf_dhistogram( ~ boot.mean, data = Dimes.boot)
# normality check
gf_qq( ~ boot.mean, data = Dimes.boot)
SE <- sd( ~ boot.mean, data = Dimes.boot); SE
# confidence interval
x.bar + 1.96 * c(0, 1) * SE

## ----dimes-boot04--------------------------------------------------------
cdata( ~ boot.mean, data = Dimes.boot)

## ----dimes-boot05--------------------------------------------------------
t.test( ~ mass, data = Dimes) %>% confint()

## ----boot-norm01, seed = 1234--------------------------------------------
S <- lapply(1:6, function(x) {rnorm(36, 100, 12)})
Boots <-
  bind_rows(
    (do(3000) * c(boot.mean = mean(resample(S[[1]])))) %>% 
      mutate(sample = "1", sample.mean = mean(S[[1]])),
    (do(3000) * c(boot.mean = mean(resample(S[[2]])))) %>% 
      mutate(sample = "2", sample.mean = mean(S[[2]])),
    (do(3000) * c(boot.mean = mean(resample(S[[3]])))) %>% 
      mutate(sample = "3", sample.mean = mean(S[[3]])),
    (do(3000) * c(boot.mean = mean(resample(S[[4]])))) %>% 
      mutate(sample = "4", sample.mean = mean(S[[4]])),
    (do(3000) * c(boot.mean = mean(resample(S[[5]])))) %>% 
      mutate(sample = "5", sample.mean = mean(S[[5]])),
    (do(3000) * c(boot.mean = mean(resample(S[[6]])))) %>% 
      mutate(sample = "6", sample.mean = mean(S[[6]]))
  )

## ----boot-norm03, fig.keep = "none"--------------------------------------
gf_dhistogram( ~ boot.mean | sample, data = Boots, binwidth = 0.5) %>%
  gf_dist("norm", mean = 100, sd = 12 / sqrt(36))

## ----boot-norm04, fig.keep = "none"--------------------------------------
gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 1)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[1]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 1") 

gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 2)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[2]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 2") 

## ----boot-norm03-fig, echo = FALSE, results = "hide", fig.keep = "last", opts.label = "fig1"----
gf_dhistogram( ~ boot.mean | sample, data = Boots, binwidth = 0.5) %>%
  gf_dist("norm", mean = 100, sd = 12 / sqrt(36))

## ----boot-norm04-fig, echo = FALSE, results = "hide", fig.keep = "all"----
gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 1)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[1]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 1") 

gf_dhistogram( ~ boot.mean | sample, binwidth = 0.5, 
               data = Boots %>% filter(sample == 2)) %>%
  gf_lims(x = c(88,112)) %>%
  gf_vline(xintercept =  mean(S[[2]])) %>%
  gf_labs(x = "3000 bootstrap means from sample 2") 

## ----boot-norm02---------------------------------------------------------
# means of samples 
sample.means <- sapply(S, mean); sample.means

## ----boot-norm05, digits = 5---------------------------------------------
# means of bootstrap distributions 
boot.means <- mean( ~ boot.mean | sample, data = Boots)
boot.means
# difference from sample means

boot.means - sample.means 

## ----boot-norm06---------------------------------------------------------
# standard error
12 / sqrt(36)
# usual standard error estimates
sapply(S, sd) / sqrt(36)
# bootstrap standard errors
sd( ~ boot.mean | sample, data = Boots)

## ----dimes-bias----------------------------------------------------------
# estimate of bias
(mean( ~ boot.mean, data = Dimes.boot) - mean( ~ mass, data = Dimes)) /
  sd( ~ boot.mean, data = Dimes.boot)

## ----boot-gamma01, seed = 1234-------------------------------------------
S <- lapply(1:6, function(x) {rgamma(16, rate = 1, shape = 2) })
Boot.Gamma <-
  bind_rows(
    (do(3000) * c(boot.mean = mean(resample(S[[1]])))) %>% 
      mutate(sample = "1", sample.mean = mean(S[[1]])),
    (do(3000) * c(boot.mean = mean(resample(S[[2]])))) %>% 
      mutate(sample = "2", sample.mean = mean(S[[2]])),
    (do(3000) * c(boot.mean = mean(resample(S[[3]])))) %>% 
      mutate(sample = "3", sample.mean = mean(S[[3]])),
    (do(3000) * c(boot.mean = mean(resample(S[[4]])))) %>% 
      mutate(sample = "4", sample.mean = mean(S[[4]])),
    (do(3000) * c(boot.mean = mean(resample(S[[5]])))) %>% 
      mutate(sample = "5", sample.mean = mean(S[[5]])),
    (do(3000) * c(boot.mean = mean(resample(S[[6]])))) %>% 
      mutate(sample = "6", sample.mean = mean(S[[6]]))
  )

## ----boot-gamma02--------------------------------------------------------
# means of samples 
sapply(S, mean) 
# means of the bootstrap distributions
mean( ~ boot.mean | sample, data = Boot.Gamma)
# standard deviations of samples 
sapply(S, sd)
# standard error of each sample
sapply(S, sd) / sqrt(16)
# standard deviations of the bootstrap distributions
sd( ~ boot.mean | sample, data = Boot.Gamma)
sqrt(1/8)
# bias
(mean( ~ boot.mean | sample, data = Boot.Gamma) - sapply(S, mean)) / 
   sd( ~ boot.mean | sample, data = Boot.Gamma)

## ----boot-gamma03, fig.keep = "none"-------------------------------------
gf_dhistogram( ~ boot.mean | sample, data = Boot.Gamma, 
               binwidth = 0.1) %>%
  gf_dist("gamma", rate = 16, shape = 32) 

## ----boot-gamma04, fig.keep = "none", include= FALSE---------------------
gf_dhistogram( ~ boot.mean | sample, binwidth = 0.1, 
               data = Boot.Gamma %>% filter(sample == 1)) %>%
  gf_vline(xintercept = mean(S[[1]]))
gf_dhistogram( ~ boot.mean | sample, binwidth = 0.1, 
               data = Boot.Gamma %>% filter(sample == 2)) %>%
  gf_vline(xintercept = mean(S[[2]]))

## ----boot-gamma03-fig, echo = FALSE, results = "hide", fig.keep = "last", opts.label = "fig1"----
gf_dhistogram( ~ boot.mean | sample, data = Boot.Gamma, 
               binwidth = 0.1) %>%
  gf_dist("gamma", rate = 16, shape = 32) 

## ----lifetime01----------------------------------------------------------
life01 <- c(16, 34, 53, 75, 93, 120, 150, 191, 240, 339)

## ----lifetime02----------------------------------------------------------
Life.boot <- do(5000) * favstats(resample(life01))
cdata(~mean, data = Life.boot, p = 0.95)
# normalized bias
(mean( ~ mean, data = Life.boot) - x.bar) / sd( ~ mean, data = Life.boot)

## ----lifetime03----------------------------------------------------------
favstats(life01)
n <- length(life01); x.bar <- mean(life01); s <- sd(life01)
Life.boot <-
  Life.boot %>%
  mutate(t = (mean - x.bar)/(sd/sqrt(n)))
q <- cdata( ~ t, data = Life.boot, p = 0.95); q
x.bar - q[2:1] * s/sqrt(n)

## ----lifetime04, fig.keep = "none", opts.label = "fig1"------------------
gf_dhistogram( ~ life01, binwidth = 50)
gf_dhistogram( ~ t, data = Life.boot, bins = 50)

## ----lifetime04-fig, echo = FALSE, results = "hide"----------------------
gf_dhistogram( ~ life01, binwidth = 50)
gf_dhistogram( ~ t, data = Life.boot, bins = 50)

## ----gpigs01-------------------------------------------------------------
gpigs <-
  c( 76,  93,  97, 107, 108, 113, 114, 119, 136, 137, 138, 139, 152, 
    154, 154, 160, 164, 164, 166, 168, 178, 179, 181, 181, 185, 194, 
    198, 212, 213, 216, 220, 225, 225, 244, 253, 256, 259, 265, 268, 
    268, 270, 283, 289, 291, 311, 315, 326, 361, 373, 376, 397, 398, 
    406, 459, 466, 592, 598)

## ----gpigs02, fig.keep = "none"------------------------------------------
gf_dhistogram( ~ gpigs, binwidth = 25)

## ----gpigs02-fig, echo = FALSE, opts.label = "fig1"----------------------
gf_dhistogram( ~ gpigs, binwidth = 25)

## ----gpigs03-------------------------------------------------------------
favstats(gpigs)
n <- length(gpigs); x.bar <- mean(gpigs); s <- sd(gpigs)
GP.boot <- 
  (do(5000) * favstats(resample(gpigs))) %>%
  mutate(t = (mean - x.bar) / (sd / sqrt(n)))
# percentile interval
cdata( ~ mean, data = GP.boot)
# normalized bias
(mean( ~ mean, data = GP.boot) - x.bar) / sd( ~ mean, data = GP.boot)

# bootstrap-t interval
q <- quantile( ~ t, data = GP.boot, p = c(0.025, 0.975)); q
x.bar - q[2:1] * s/sqrt(n)

## ----boott-sims01--------------------------------------------------------
ci3 <- function(x, r = 1000, conf.level = 0.95) {
  x.bar <- mean(x); n <- length(x) 
  sqrtn <- sqrt(n); SE <- sd(x) / sqrtn
  Boot <- do(r) * {
    rx <- resample(x)
    mean.boot <- mean(rx)
    se.boot <- sd(rx) / sqrtn
    c(mean.boot = mean.boot,
         t.boot = (mean.boot - x.bar) / se.boot
    )
  }
  q <- cdata(~ t.boot, data = Boot, 0.95)
  tint <- stats::t.test(x) %>% confint()
  data.frame(
    method = c("percentile", "bootstrap-t", "t"),
    estimate = x.bar,
    lo = c(
      quantile(Boot$mean.boot, 0.025),
      x.bar - q[2] * SE,
      tint$lower),
    hi = c(  
      quantile(Boot$mean.boot, 0.975),
      x.bar
      - q[1] * SE,
      tint$upper)
    )
}
# eample use
ci3(rgamma(20, 2, 1))

## ----boott-sims02, fig.keep = "none", seed = 12345-----------------------
Sims <-
  (do(400) * ci3(rgamma(20, 1, 1/2))) %>%
  mutate(
    status = c("lo", "good", "hi")[1 + (2 <= lo) + (2 < hi)])

## ----boott-sims03, fig.keep = "none"-------------------------------------
df_stats(status ~ method, data = Sims, props)

## ----boott-sims03-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
gf_linerange(lo + hi ~ .index, data = Sims,
             color = ~ status, size = 0.5, alpha = 0.7) %>%
  gf_point(estimate ~ .index, size = 0.5, alpha = 0.7) %>%
  gf_facet_grid(method ~ .) %>%
  gf_refine(scale_color_manual(
    values = c(good = "gray70", hi = "red", lo = "navy"),
    breaks = c("hi", "good", "lo")
    )) %>%
  gf_labs(x = "")

## ----bodytemp------------------------------------------------------------
Lock5withR::BodyTemp50$BodyTemp

## ----boot-boys01---------------------------------------------------------
# select the 10 year old boys
Boys10 <- 
  NHANES::NHANES %>%
  filter(Age == 10, Gender == "male") %>%
  select(Age, Gender, Height) 

favstats( ~ Height, data = Boys10)

## ----boot-boys02---------------------------------------------------------
# create the bootstrap distribution
Boys.boot.sd <- 
  do(3000) * c(sd.boot = sd( ~ Height, data = resample(Boys10)))
# check for biased estimator
(mean( ~ sd.boot, data = Boys.boot.sd) - sd( ~ Height, data = Boys10)) / 
  sd( ~ sd.boot, data = Boys.boot.sd)

## ----boot-boys03---------------------------------------------------------
# create the bootstrap distribution
Boys.boot <- 
  do(3000) * c(var.boot = var( ~ Height, data = resample(Boys10)))
# check for biased estimator
(mean( ~ var.boot, data = Boys.boot) - var( ~ Height, data = Boys10)) / 
  sd( ~ var.boot, data = Boys.boot)

## ----boot-boys04---------------------------------------------------------
cdata( ~ var.boot, data = Boys.boot)
cdata( ~ var.boot, data = Boys.boot)[1:2] %>% sqrt()

## ----boot-boys05---------------------------------------------------------
cdata( ~ sd.boot, data = Boys.boot.sd)

## ----dimes, include = FALSE, tidy = FALSE--------------------------------
s <- sd(~ mass, data = Dimes)
n <- nrow(Dimes)
B <- 10200; B
D <- mean( ~ mass, data = Dimes); D
uB <- 100 / sqrt(12); uB
uD <- s / sqrt(n); uD
u <- sqrt( 1/D^2 * uB^2 + B^2/D^4 * uD^2  )

## ----propagation-unif01--------------------------------------------------
X <- runif(100000, 0, 1)
Y <- sqrt(X)
mean(Y)
var(Y)

## ----propagation-unif01-sol, warning = FALSE-----------------------------
integrate(makeFun( y * 2*y ~ y), 0, 1)
mu <- integrate(function(y) y * 2*y, 0, 1) %>% value()
fractions(mu)
integrate(function(y) (y-mu)^2 * 2*y, 0, 1)
integrate(function(y) (y-mu)^2 * 2*y, 0, 1) %>% value() %>% fractions()

## ----dimes-favstats------------------------------------------------------
favstats( ~ mass, data = Dimes)

## ----mean-dime-cl--------------------------------------------------------
pt(1, df = 29) - pt(-1, df = 29)

## ----dimes-sim, digits = 4-----------------------------------------------
B <- runif(10000, 10150, 10250)
Dimes.boot <- do(10000) * mean( ~ mass, data = resample(Dimes))
head(Dimes.boot, 3)
Dimes.boot <- 
  Dimes.boot %>% mutate(D = mean, N = B / D)
gf_dhistogram( ~ N, data = Dimes.boot)
gf_qq( ~ N, data = Dimes.boot)
sd( ~ N, data = Dimes.boot)

## ----resistors, include=FALSE--------------------------------------------
R <- 20*50/(20 + 50)
u1 <- 0.7; u2 <- 1.2
p1 <- (50/70)^2; p2 <- (20/70)^2
u <- sqrt( p1^2 * u1^2 + p2^2 *u2^2 )
r <- 1 + round(-log10(u))

## ----rm-pi-sol, include=FALSE--------------------------------------------
rm(pi)

## ----tank-sol, tidy=FALSE------------------------------------------------
L <- 2.65; W <- 3.10; H <- 4.61
uL <- 0.02; uW <- 0.02; uH <- 0.05
V <- L * W * H; V
uV <- sqrt( (uL/L)^2 + (uW/W)^2 + (uH/H)^2) * V; uV

## ----relative-uncertainty-sol, tidy=FALSE--------------------------------
V <- 1.637 / 0.43; V
uV <- sqrt( (0.02/.43)^2 + (0.006/1.637)^2 ) * V; uV


## ----Likelihood, child="Likelihood.Rnw", eval=includeChapter[5]----------

## ----lik-setup, include = FALSE, cache = FALSE---------------------------
knitr::opts_chunk$set(cache.path = "cache/Lik-") 
require(maxLik)

## ----dice-likelihood-sol-------------------------------------------------
expand.grid(
  x = 0:10, size = 10, sides = c(4, 6, 10)) %>%
  mutate(
    die = factor(paste("D", sides, sep = ""), 
                 levels = c("D4", "D6", "D10")),
    probability = dbinom(x, size, 2 / sides)) %>%
  gf_point(probability ~ x, color = ~ die) %>%
  gf_line(probability ~ x, color = ~ die) 

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
gf_line(llik(xpts) ~ xpts) %>%
  gf_labs(x = expression(pi), y = "log-likelihood")

## ----mle-unif01-fig, echo = FALSE, results = "hide"----------------------
L <- function(theta) { (1 / theta)^6 * (theta >= 8.7) }
plotFun(L(theta) ~ theta, theta.lim = c(6, 15),
        xlab = expression(theta), ylab = expression(L(theta)))

## ----zero-one-mom-mle----------------------------------------------------
x <- 
  c(0.90, 0.78, 0.93, 0.64, 0.45, 
    0.85, 0.75, 0.93, 0.98, 0.78)
mean(x)
mom <- (1 / (1 - mean(x))) - 2; mom
mle <- ( - length(x) / sum(log(x))) - 1; mle

## ----plant-density01-sol-------------------------------------------------
# plant densities (lambda) for simulations
density <- c(0.01, 0.1, 0.25, 0.5, 1, 2, 4, 10, 100)       
sizes <- c(10, 20, 50)
simulate <- 
  function(lambda = 1, size = 10, area = 1, 
           method = c("count", "distance")){
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
  data_frame(
    size = size, lambda = lambda, method = method,
    estimate = mle, plants = plants, area = total.area,
    lambdaFac = paste0("l=", lambda),
    sizeFac = paste0("size=", size)
  )
}

## ----plant-density02-sol, tidy=FALSE-------------------------------------
Results <- c()  
for (lambda in density) {
  for (size in sizes) {
    Results <- bind_rows(Results, 
	  do(1000) * simulate(lambda, size, method = "count"), 
	  do(1000) * simulate(lambda, size, method = "distance") 
	) 
  }
}
Results <- 
  Results %>% 
  mutate(lambdaFac = reorder(lambdaFac, lambda))

## ----plant-density-bakeoff01-sol, tidy=FALSE-----------------------------
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

## ----plant-density-bakeoff02-sol, eval=FALSE-----------------------------
## log(estimate / lambda)

## ----plant-density-plot01-sol, fig.height=8, fig.width = 7, tidy=FALSE----
gf_jitter( 
  method ~ log(estimate / lambda) | lambdaFac ~ sizeFac, 
  data = Results, alpha = 0.1)

## ----plant-density-plot02-sol, fig.height=8, fig.width = 7, tidy=FALSE, warning=FALSE----
gf_violin(
  log(estimate / lambda) ~ method, data = Results,
  color = ~ method, fill = ~ method, alpha = 0.5, adjust = 2) %>%
  gf_refine(annotate("hline", yintercept = 0), coord_flip()) %>%  
  gf_facet_grid(lambdaFac ~ sizeFac) 

## ----batting-average, echo = FALSE, results = "asis"---------------------
set.seed(1234) 
Batters2015 <- 
  Lahman::Batting %>% 
  filter(yearID == 2015) %>% 
  group_by(playerID) %>% 
  summarise(
    G=sum(G, na.rm = TRUE), 
    AB = sum(AB, na.rm = TRUE), 
    H = sum(H, na.rm = TRUE)) %>% 
  filter(AB >= 200) %>% 
  mutate(BA = round(H/AB, 3)) 
SampleBatters <- 
  Batters2015 %>%
  sample_n(16)
cat(paste(format(SampleBatters$BA[1:8], digits = 3), collapse = " & "))
cat("\\\\")
cat(paste(format(SampleBatters$BA[9:16], digits = 3), collapse = " & "))

## ----baseball-ba01, tidy = FALSE-----------------------------------------
ba <- c(0.276, 0.281, 0.225, 0.283, 0.257, 0.250, 0.250, 0.261, 
        0.312, 0.259, 0.273, 0.222, 0.314, 0.271, 0.294, 0.268)

## ----baseball-ba02, tidy = FALSE-----------------------------------------
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

## ----baseball-ba03-------------------------------------------------------
require(maxLik)
ml  <- maxLik(loglik,  start = c(shape1 = 1, shape2 = 1), x = ba) 
ml2 <- maxLik(loglik2, start = c(shape1 = 1, shape2 = 1), x = ba)
ml
ml2
# get just the estimated parameter values
coef(ml)
# get just the "return message" -- always good to check
returnMessage(ml)
alpha.hat <- coef(ml)[1]
 beta.hat <- coef(ml)[2]

## ----baseball-ba01-fig, echo=FALSE---------------------------------------
gf_dist("beta", shape1 = alpha.hat, shape2 = beta.hat)

## ----baseball-ba04-------------------------------------------------------
qbeta(c(0.05, 0.95), shape1 = alpha.hat, shape2 = beta.hat)

## ----baseball-ba05-------------------------------------------------------
# using the ruler method
qbeta(    0.5 / 334, alpha.hat, beta.hat)
qbeta(1 - 0.5 / 334, alpha.hat, beta.hat)

## ----baseball-ba-likelihood-fig, echo = FALSE, include = FALSE-----------
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
  xlab = list(label = expression(alpha), cex = 0.7),
  ylab = list(label = expression(beta), cex = 0.7),
  zlab = "",
  scale = list(arrows = FALSE, cex = 0.5, z = list(draw = FALSE))
)

dat <- expand.grid(
	alpha = seq(4, 325, by = 2),
	beta = seq(10, 800, by = 4)
	) 

dat$loglik <- apply(cbind(dat$alpha, dat$beta), 1, FUN = "loglik", x = ba)

levelplot(loglik ~ alpha + beta, data = dat,
          xlab = expression(alpha),
          ylab = expression(beta),
          main = "log-likelihood", 
          col.regions = topo.colors(n=100),
          panel = function(x, y, z, ...){
            panel.levelplot(x, y, z, ...)
            panel.xyplot(x = alpha.hat, y = beta.hat, ...)
          }
)

## ----ba-boot01-sol-------------------------------------------------------
loglik2 <- function(theta, x) { 
  if (any(theta <= 0)) 
    NA     # alert maxLik regarding parameter values that are not allowed
  else
    dbeta(x, theta[1], theta[2], log = TRUE) 
}
BA.boot <-
  do(2000) * coef(maxLik(loglik2, x = resample(ba), 
                         start = c(shape1 = 100, shape2 = 100)))

## ----ba-boot02-sol, warning = FALSE--------------------------------------

# coefficients from original sample
cc <- 
  coef(maxLik(loglik2, x = ba, start = c(shape1 = 100, shape2 = 100)))
# normalized bias estimates
(mean( ~ shape1, data = BA.boot) - cc["shape1"]) / 
   sd( ~ shape1, data = BA.boot)
(mean( ~ shape2, data = BA.boot) - cc["shape2"]) / 
   sd( ~ shape2, data = BA.boot)
# 95% confidence intervals
cdata( ~ shape1, data = BA.boot)
cdata( ~ shape2, data = BA.boot)
# histograms of bootstrap distributions
gf_dhistogram( ~ shape1, data = BA.boot, binwidth = 20) %>% 
  gf_lims(x = c(0, 500))
gf_dhistogram( ~ shape2, data = BA.boot, binwidth = 50) %>%
  gf_lims(x = c(0, 1000))

## ----ba-boot03-sol-------------------------------------------------------
# normalized bias estimate
(mean( ~ shape2 / shape1, data = BA.boot) - 
   cc["shape2"]/cc["shape1"]) / 
   sd( ~ (shape2 / shape1), data = BA.boot)
# 95% CI
cdata( ~ (shape2 / shape1), data = BA.boot)
# histogram of bootstrap distribution
gf_dhistogram( 
  ~ (shape2 / shape1), data = BA.boot, v = cc["shape2"]/cc["shape1"])

## ----ba-boot04-sol-------------------------------------------------------
cdata( ~ (shape2 / shape1), data = BA.boot) / 
         (cc["shape2"]/cc["shape1"]) 
cdata( ~ shape1, data = BA.boot) / cc["shape1"]
cdata( ~ shape2, data = BA.boot) / cc["shape2"]

## ----ba-boot05-sol-------------------------------------------------------

alpha <- 0.5 / 334
BA.boot <- BA.boot %>%
  mutate(
    min = qbeta(    alpha, shape1 = shape1, shape2 = shape2),
    max = qbeta(1 - alpha, shape1 = shape1, shape2 = shape2))

min_est <- 
  qbeta(    alpha, shape1 = cc["shape1"], shape2 = cc["shape2"])
max_est <- 
  qbeta(1 - alpha, shape1 = cc["shape1"], shape2 = cc["shape2"])

# normalized bias
(min_est - mean( ~ min, data = BA.boot)) / sd( ~ min, data = BA.boot)
(max_est - mean( ~ max, data = BA.boot)) / sd( ~ max, data = BA.boot)

gf_dhistogram( ~ min, data = BA.boot)
gf_dhistogram( ~ max, data = BA.boot)
cdata( ~ min, data = BA.boot, 0.95)
cdata( ~ max, data = BA.boot, 0.95)

## ----ba-boot06-sol-------------------------------------------------------
BA.boot2 <- do(2000) * favstats( resample(ba))
(mean( ~ mean, data = BA.boot2) - mean(ba)) / sd( ~ mean, data = BA.boot2)
confint(BA.boot2, method = "percentile", parm = "mean")
confint(BA.boot2, method = "bootstrap-t")

## ----normal-loglik-------------------------------------------------------
loglik.normal <- function(theta, x) {
  mu <- theta[1]; sigma <- theta[2]
  if (sigma < 0) return(NA)     # alert maxLik() to invalid values of sigma
  dnorm(x, mu, sigma, log = TRUE)
}

## ----normal-mle01--------------------------------------------------------
x <- rnorm(30, 100, 10)
maxLik(loglik.normal, start = c(mu = 0, sigma = 1), x = x)

## ----normal-mle02--------------------------------------------------------
MLEs <-
  do(5000) * coef(maxLik(loglik.normal, 
                         start = c(mu = 0, sigma = 1), 
                         x = rnorm(30, 100, 10)))
head(MLEs, 3)
gf_dhistogram( ~ mu,    data = MLEs, binwidth = 0.5) %>%
  gf_labs(x = expression(hat(mu)))
gf_dhistogram( ~ sigma, data = MLEs, binwidth = 0.5) %>%
  gf_labs(x = expression(hat(sigma)))
gf_qq( ~ mu, data = MLEs, geom = "line") %>%
  gf_labs(y = expression(hat(mu)))
gf_qq( ~ sigma, data = MLEs, geom = "line") %>%
  gf_labs(y = expression(hat(sigma)))

## ----normal-mle03--------------------------------------------------------
gf_dhistogram( ~ sigma^2, data = MLEs, binwidth = 5)
gf_qq( ~ sigma^2, data = MLEs, geom = "line", 
         distribution = qchisq, dparams = list(df = 29)) %>%
  gf_labs(x = "Chisq(29)", y = expression(hat(sigma)^2))

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

gf_dhistogram( ~ duration, data = geyser, binwidth = 0.20, alpha = 0.5) %>%
  gf_function(fun = dmix, 
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

gf_dhistogram( ~ duration, data = geyser, binwidth = 0.20, alpha = 0.5) %>%
  gf_function(fun = dmix, 
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
# Nelder-Mead doesn't converge (fast enough)
maxLik(loglik.faithful, x = geyser$duration, method = "NM",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s)) 
# Nelder-Mead converges if we give it more time
maxLik(loglik.faithful, x = geyser$duration, method = "NM", 
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s), 
       control = list(iterlim = 3000))
# BFGS "converges", but only fits one group
maxLik(loglik.faithful, x = geyser$duration, method = "BFGS",
       start = c(alpha = 0.5, mu1 = m, mu2 = m, sigma1 = s, sigma2 = s))

## ----grid01, digits = 5--------------------------------------------------
x <- c(26.65, 28.03, 35.55, 29.30, 29.54, 36.20, 30.94, 
       23.69, 26.12, 27.13, 34.14, 30.51, 30.68, 29.46, 
       26.67, 36.51, 31.09, 20.74, 31.95, 27.01)

# Note the use of mapply() below.  
#   This tells R to use ALL the values of x with EACH combination of m and s.
Grid <-
  expand.grid(
    mean = seq(10, 100, by = 0.1),
    sd = seq(1, 10, by = 0.1)) %>% 
  mutate(
    loglik = mapply(function(m, s) { sum(dnorm(x, m, s, log = TRUE)) },
                    m = mean, s = sd))
Grid %>% arrange(-loglik) %>% head(3) 

## ----grid02, digits = 5--------------------------------------------------
Grid2 <-
  expand.grid( mean = seq(29.4, 29.8, by = 0.001),
               sd = seq(3.8, 4.2, by = 0.001)) %>% 
  mutate(loglik = mapply(function(m, s) { sum(dnorm(x, m, s, log = TRUE)) },
                      m = mean, s = sd))
Grid2 %>% arrange(-loglik) %>% head(3) 

## ----grid03, digits = 5--------------------------------------------------
mean( ~ x)
sd (~ x) * sqrt(19/20)

## ----grid04--------------------------------------------------------------
# number of function evaluations used in this example
nrow(Grid) + nrow(Grid2)

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
# works if we select a good starting point -- 
#   but warns about boundary issues.
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
gf_point(y1 ~ theta, size = 0.05) %>%
  gf_labs(x = expression(theta),
          y = "likelihood")
gf_point(y2 ~ theta, size = 0.05) %>%
  gf_labs(x = expression(theta),
          y = "log-likelihood")

## ----hwe-mle-sol---------------------------------------------------------
theta2probs <- function(theta) { 
    c(theta^2, 2*theta*(1-theta), (1-theta)^2)  
}
loglik.hwe <- function(theta, x) {
  probs <- theta2probs(theta)
  if (any(probs < 0 )) { return(NA) }
	dmultinom(x, sum(x), theta2probs(theta), log = TRUE)
}

geno <- c(83, 447, 470)
maxLik(loglik.hwe, start = 0.5, x = geno)

## ----mix-normals03-sol---------------------------------------------------
0.3 * pnorm(12, 8, 2) + 0.7 * pnorm(12, 16, 3)

## ----mix-normals04-sol---------------------------------------------------
Plot_data <-
  data_frame(
    x = seq(0, 30, by = 0.10),
    density = 0.3 * dnorm(x, 8, 2) + 0.7 * dnorm(x, 16, 3),
    density1 = 0.3 * dnorm(x, 8, 2),
    density2 = 0.7 * dnorm(x, 16, 3)
    )
gf_line(density ~ x, data = Plot_data, size = 1.5, alpha = 0.5) %>%
  gf_line(density1 ~ x, data = Plot_data, color = "red") %>%
  gf_line(density2 ~ x, data = Plot_data, color = "blue") %>%
  gf_labs(title = "pdf of a mixture of normals")

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
plot(ml.pois10) %>% gf_labs(title = "n = 10")

## ----pois-lrt03----------------------------------------------------------
-hessian(ml.pois10)   # I = - hessian
stdEr(ml.pois10)      # I^(-1/2)(theta.hat)
(-hessian(ml.pois10))^(-1/2)

## ----pois-lrt04, fig.keep = "none"---------------------------------------
ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) %>% gf_labs(title = "n = 100")

## ----pois-lrt04-fig, echo = FALSE----------------------------------------
# We can express l() in terms of sufficient statistics 
loglik.pois <- function(theta, x.bar = 1.4, n = 10) {
  - n * theta + n * x.bar * log(theta)
}
ml.pois10 <- 
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 10)
plot(ml.pois10) %>% gf_labs(title = "n = 10")
ml.pois100 <-
  maxLik2(loglik.pois, start = c(lambda = 1), x.bar = 1.4, n = 100) 
plot(ml.pois100) %>% gf_labs(title = "n = 100")

## ----pois-lrt05----------------------------------------------------------
-hessian(ml.pois100)     # information
stdEr(ml.pois100)        

## ----pois-mle, seed = 123------------------------------------------------
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

## ----pois-ci-plot-fig, tidy = FALSE, echo = FALSE, cache = FALSE---------
plot(ml.pois10, ci = c("wald", "li"), hline = TRUE)  

## ----wald-ci01, include = FALSE------------------------------------------
x <- 35; n <- 55
pi.hat <- x / n; pi.hat
SE <- sqrt(pi.hat * (1 - pi.hat) / n); SE
pi.hat + c(-1, 1) * qnorm(0.975) * SE

## ----binom-wald-ci-------------------------------------------------------
x <- 35; n <- 55
pi.hat <- x / n; pi.hat
SE <- sqrt(pi.hat * (1 - pi.hat) / n); SE
pi.hat + c(-1, 1) * qnorm(0.975) * SE

## ----binom-lci, tidy = FALSE---------------------------------------------
loglik.binom <-  function(p, x, n) { 
  ifelse (p < 0 | p > 1, NA, x * log(p) + (n-x) * log(1 - p))
}
pval_minus_critical <- function(pi0) {
    2 * (loglik.binom(pi.hat, x, n) - loglik.binom(pi0, x, n)) - 
    qchisq(.95, df = 1)}
lo <- uniroot( pval_minus_critical, c(0, pi.hat)) %>% value()
hi <- uniroot( pval_minus_critical, c(pi.hat, 1)) %>% value()
c(lo, hi)

## ----binom-ci-compare-fig, echo = FALSE, warning = FALSE, cache = FALSE----
ml.binom <- maxLik2(loglik.binom, x = 35, n = 55, start = 0.5) 
plot(ml.binom, ci = c("w", "l"), hline = TRUE) %>%
  gf_labs(x = expression(pi))

## ----binom-odds-ci, tidy = FALSE-----------------------------------------
loglik.binom2 <- function(theta, x, n) {
  x * log(theta / (1 + theta)) + (n - x) * log(1 / (1 + theta))
}
ml.binom2 <- maxLik2(loglik.binom2, start = (odds = 1), x = 35, n = 55)
coef(ml.binom2)
x <- 35; n <- 55; theta.hat <- 35 / 20; theta.hat
pval_minus_critical2 <- function(theta0) {
    2 * (loglik.binom2(theta.hat, x, n) - loglik.binom2(theta0, x, n)) - 
    qchisq(.95, df = 1)
  } 
lo2 <- 
  uniroot(pval_minus_critical2, c(0, theta.hat)) %>% value()
hi2 <- 
  uniroot(pval_minus_critical2, c(theta.hat, 100)) %>% value()

c(lo2, hi2)
c(lo2, hi2) / (1 + c(lo2, hi2))
c(lo, hi)    # interval computed previously, for comparison

## ----binom-odds-ci-fig, echo = FALSE, warning = FALSE--------------------
plot(ml.binom2, hline = TRUE) %>%
  gf_lims(y = c(-45, -35)) %>%
  gf_labs(x = "log odds")

## ----logodds01-sol-------------------------------------------------------
loglik.binom3 <- function(logodds, x, n) {
  odds <- exp(logodds)
  p <- odds / (1 + odds)
  x * log(p) + (n -x) * log(1 - p)
}
ml.binom3 <- 
  maxLik2(loglik.binom3, x = 35, n = 55, start = c(logodds = 0))

logodds.hat <- coef(ml.binom3); logodds.hat

# W = 2 * difference in log likelihoods
W <- function(logodds) {
  2 * (loglik.binom3(logodds.hat, x = 35, n = 55) - 
       loglik.binom3(logodds,         x = 35, n = 55))
}

# W = 2 * difference in log likelihoods
pv <- function(logodds) {
  1 - pchisq(2 * (loglik.binom3(logodds.hat, x = 35, n = 55) - 
       loglik.binom3(logodds,         x = 35, n = 55)), df = 1)
}

# find endpoints of rejection region using W
lo <- uniroot(function(logodds) W(logodds) - qchisq(0.95, df = 1), 
              c(-100, logodds.hat)) %>% value()
hi <- uniroot(function(logodds) W(logodds) - qchisq(0.95, df = 1), 
              c(10, logodds.hat)) %>% value()

# find endpoints of rejection region using pval
lo2 <- uniroot(function(logodds) pv(logodds) - 0.05,
              c(-100, logodds.hat)) %>% value()
hi2 <- uniroot(function(logodds) pv(logodds) - 0.05, 
              c(10, logodds.hat)) %>% value()

# Likelihood interval for log odds
c(lo, hi)
c(lo2, hi2)

# Wald interval for log odds
logodds.hat + c(-1, 1) * qnorm(0.975) * stdEr(ml.binom3)

## ----logodds02-sol, tidy = FALSE, warning=FALSE--------------------------
plot(ml.binom, hline = TRUE) %>%
  gf_lims(y = c(-42, -35.5)) %>%
  gf_labs(title = "parameter: proportion")
plot(ml.binom2, hline = TRUE) %>%
  gf_lims(y = c(-42, -35.5)) %>%
  gf_labs(title = "parameter: odds")
plot(ml.binom3, hline = TRUE) %>%
  gf_lims(y = c(-42, -35.5)) %>%
  gf_labs(title = "parameter: log odds")

## ----logodds03-sol-------------------------------------------------------
pi.hat <- coef(ml.binom); pi.hat
odds.hat <- pi.hat / (1 - pi.hat); odds.hat
coef(ml.binom2)
log(odds.hat)
coef(ml.binom3)

## ------------------------------------------------------------------------
l <- 
  function(lambda, x.bar = 1.4, n = 10)  
    -n * lambda  + log(lambda)  * n * x.bar

l.star <- 
  function(theta,  x.bar = 1.4, n = 10)  
    -n * 1/theta + log(1/theta) * n * x.bar

ml      <- maxLik(l,      start = c(lambda = 1))
ml.star <- maxLik(l.star, start = c(theta  = 1))

coef(ml)
1/coef(ml)
coef(ml.star)
uniroot(
  function(lambda0) 2 * (l(coef(ml)) - l(lambda0)) - qchisq(.95, 1), 
  c(0, 1.4)) %>% value()
1/ uniroot(
  function(lambda0) 
    2 * (l(coef(ml)) - l(lambda0)) - qchisq(.95, 1), 
  c(0, 1.4)) %>% value()
uniroot(
  function(theta0) 
    2 * (l.star(coef(ml.star)) - l.star(theta0)) - qchisq(.95, 1), 
  c(1/1.4, 10)) %>% value()

## ----faithful-lrt01------------------------------------------------------
data(geyser, package = "MASS")
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
logLik(ml)        # makLik::logLik can caclulate this log-likelihood for us

ml0 <- maxLik(loglik0.faithful, x = geyser$duration,
              start = c(m - 1, m + 1, s, s))
mle0 <- coef(ml0); mle0
logLik(ml0)                      
lrt.stat <- 2 * (logLik(ml) - logLik(ml0)); lrt.stat
1 - pchisq(lrt.stat, df = 1)     # p-value based on asymptotic distribution

## ----faithful-lrt03, echo = FALSE----------------------------------------
gf_dhistogram( ~ duration, data = geyser, binwidth = 0.20, alpha = 0.2,
               fill = "navy") %>%
  gf_function(fun = dmix, 
              args = list(
                alpha =  mle[1],
                mu1 =    mle[2], mu2 =    mle[3],
                sigma1 = mle[4], sigma2 = mle[5]),
                color = "gray30"
  ) %>%
  gf_function(fun = dmix, 
              args = list(
                alpha =  0.5,
                mu1 =    mle0[1], mu2 =    mle0[2],
                sigma1 = mle0[3], sigma2 = mle0[4]),
                linetype = "dashed"
  )

## ----faithful-lrt04, tidy = FALSE----------------------------------------
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
plaplace1(3, lambda = 1, theta = 2)
plaplace1(3, lambda = 1, theta = 2) - plaplace1(0, lambda = 2, theta = 1)
plaplace2(3, lambda = 1, theta = 2)
plaplace2(3, lambda = 1, theta = 2) - plaplace2(0, lambda = 2, theta = 1)

## ----laplace-mle01-sol---------------------------------------------------
x <- 
  c(1.00, -1.43, 0.62, 0.87, -0.66, -0.59, 1.30, -1.23, -1.53, -1.94)

loglik.laplace <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(0.5) + dexp(abs(x-m), rate = lambda, log = TRUE)))
}

## ----laplace-mle02-sol---------------------------------------------------
ml.laplace <- maxLik(loglik.laplace, start = c(0, 1), x = x)
ml.laplace

## ----laplace-mle03-sol---------------------------------------------------
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
x <- 
  c(1.00, -1.43, 0.62, 0.87, -0.66, -0.59, 1.30, -1.23, -1.53, -1.94)
loglik.laplace1 <- function(theta, x) {
    m <- theta[1]; lambda <- theta[2]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}
loglik.laplace0 <- function(theta, x) {
    m <- 0; lambda <- theta[1]
    return(sum(log(0.5) + dexp(abs(x - m), rate = lambda, log = TRUE)))
}

## ----laplace-lrt02-------------------------------------------------------
free <- 
  maxLik(loglik.laplace1, start = c(m = 0, lambda = 1), x = x); free  
free.est <- coef(free)

## ----laplace-lrt03-------------------------------------------------------
null <- 
  maxLik(loglik.laplace0, start = c(lambda = 1), x = x); null
null.est <- coef(null)

## ----laplace-lrt04-------------------------------------------------------
w <- 
  2 * (loglik.laplace1(free.est, x) - loglik.laplace0(null.est, x)) 
w
1 - pchisq(w, df = 1)          # p-value based on asymptotic distribution

## ----less-lazy00, include = FALSE----------------------------------------
theta <- 1.8
set.seed(123)
rfoo <- function(n, theta) {runif(n)^(1 / (theta + 1))}
pfoo <- function(q, theta) {
  q^(theta + 1)
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
gf_dhistogram(~ x, binwidth = 0.1) %>%
  gf_dist("foo", theta = 1.8)

## ----less-lazy-sol-------------------------------------------------------
x <- 
  c(0.64, 0.92, 0.73, 0.96, 0.98, 0.33, 0.80, 0.96, 0.81, 0.76, 
    0.98, 0.75, 0.87, 0.82, 0.44, 0.96, 0.61, 0.32, 0.67, 0.98, 
    0.96, 0.88, 0.85, 1.00, 0.86, 0.88, 0.80, 0.83, 0.64, 0.50)
n <- length(x); n
theta.hat <- -n / sum(log(x)) - 1; theta.hat
W_obs <- 2 * (n * log(theta.hat + 1) + theta.hat * sum(log(x))); W_obs
1 - pchisq(W_obs, df = 1)

## ------------------------------------------------------------------------
one_sim <- function(n = 30L) {
  x <- runif(n)
  theta.hat <- -n / sum(log(x)) - 1; theta.hat
  W <- 2 * (n * log(theta.hat + 1) + theta.hat * sum(log(x)))
  data_frame(n = n, theta.hat = theta.hat, W = W, pval = 1 - pchisq(W, df = 1))
}

Sims <- do(5000) * one_sim()
head(Sims)
# estimated p-value 
# more replications needed to approximate better
# but this crude estimate is at least consistent with the result above
prop1( ~(W >= W_obs), data = Sims)
binom.test( ~(W >= W_obs), data = Sims)

## ------------------------------------------------------------------------
gf_dhistogram(~ W, data = Sims, bins = 100) %>%
  gf_dist("chisq", df = 1, color = "red") %>%
  gf_lims(y = c(0,2))
gf_qq( ~ W, data = Sims, distribution = "qchisq", dparams = list(df = 1))
df_stats( ~ (W >= W_obs), data = Sims, props)

## ----faithful-ci, tidy = FALSE, warning=FALSE----------------------------
# loglik defined above   
data(geyser, package = "MASS")
snippet("faithful-mle01", echo = FALSE)
snippet("faithful-mle02", echo = FALSE)
m <- mean( ~ duration, data = geyser)
s <-   sd( ~ duration, data = geyser)
ml.faithful <- maxLik(loglik.faithful, x = geyser$duration,
             start = c(0.5, m - 1, m + 1, s, s))
mle <- coef(ml.faithful)
p <- function(a) {
  ml.faithful.a <- maxLik(loglik.faithful, x = geyser$duration,
                start = c(a, m - 1, m + 1, s, s), 
                fixed = 1)
  lrt.stat <- 2 * (logLik(ml.faithful) - logLik(ml.faithful.a)) 
  pval <- 1 - pchisq(lrt.stat, df = 1)         
  return(pval)
}
lo <- uniroot(function(a){p(a) - 0.05}, c(0.1, mle[1])) %>% value(); lo
hi <- uniroot(function(a){p(a) - 0.05}, c(0.9, mle[1])) %>% value(); hi

## ----golfballs-max, eval = TRUE, tidy = FALSE, fig.show = "hide"---------
golfballs <- c(137, 138, 107, 104)  
statTally(golfballs, rgolfballs, max, 
          xlab = "test statistic (max)")

## ----golfballs-max-fig, echo = FALSE, message = FALSE, results = "hide"----
golfballs <- c(137, 138, 107, 104)  
statTally(golfballs, rgolfballs, max, 
          xlab = "test statistic (max)")

## ----golfballs-lrt1, digits = 4------------------------------------------
# LRT calculation
o <- golfballs; o  
e <- rep(486 / 4, 4); e
G <- 2 * sum (o * log(o / e)); G       # lrt Goodness of fit statistic
1 - pchisq(G, df = 3)

## ----golfballs-lrt2------------------------------------------------------
# function to compute G statistic from tabulated data
G <- function(o) {e <- rep(486 / 4, 4); 2 * sum (o * log(o / e))}
statTally(golfballs, rgolfballs, G)  

## ----golfballs-pearson01-------------------------------------------------
E <- rep(486 / 4, 4)  
chisqstat <- function(x) { sum((x - E)^2 / E) }
statTally(golfballs, rgolfballs, chisqstat, xlab = expression(X^2))

## ----golfballs-pearson02, digits = 4-------------------------------------
# manual calculation
o <- golfballs; o
e <- rep(486 / 4, 4); e
X <- sum ((o - e)^2 / e); X
1 - pchisq(X, df = 3)
# repeated using built-in method
chisq.test(o)

## ----golfballs-pearson03-------------------------------------------------
chisq.test(golfballs, simulate.p.value = TRUE, B = 10000)

## ----golfballs-complex01-------------------------------------------------
o
a <- sum(o[1:2]) / (2 * sum(o)); a
b <- sum(o[3:4]) / (2 * sum(o)); b
a + b                                # should equal 0.5
lnum <- 275 * log(a) + 211 * log(b)
ldenom <- sum(o * log (o/ sum(o)))
G <- -2 * (lnum - ldenom); G
1 - pchisq(G, df = 2)

## ----golfballs-complex02-------------------------------------------------
e <- c(a, a, b, b) * 486; e
2 * sum(o * log(o/e))  # lrt, same as above
sum( (o - e)^2 / e)    # Pearson

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
GOF(data, cutpts = c(0, 3, 6, 12, Inf), iterlim = 1000, 
    start = c(5, 5))$table
GOF(data, cutpts = c(0, 3, 6, 12, Inf), iterlim = 1000, start = c(5, 5))
GOF(data, cutpts = c(0, 3, 6, 12, Inf), iterlim = 1000, start = c(5, 5), 
    pearson = TRUE)

## ----gof-gamma-----------------------------------------------------------
oldopt <- options(warn = -1)
data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
gamlik <- 
  function(theta, x) { sum(dgamma(x, theta[1], theta[2], log = TRUE)) }
pgamm <- 
  function(x, theta){ pgamma(x, theta[1], theta[2]) }
GOF(data, gamlik, pgamm, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf))$table
GOF(data, gamlik, pgamm, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf))
GOF(data, gamlik, pgamm, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf), 
    pearson = TRUE)
options(oldopt)

## ----gof-weibull, warning=FALSE------------------------------------------
data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
weiblik <- 
  function(theta, x) { sum(dweibull(x, theta[1], theta[2], log = TRUE)) }
pweib <- function(x, theta){ pweibull(x, theta[1], theta[2]) }
GOF(data, weiblik, pweib, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf))$table
GOF(data, weiblik, pweib, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf))
GOF(data, weiblik, pweib, start = c(1, 1), cutpts = c(0, 3, 6, 12, Inf), 
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

## ----hwe-gof-man-sol-----------------------------------------------------
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
X <- chisq.test(fisher.counts, p = theta2probs(theta.hat)) %>% stat; X
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

## ----family-smoking02----------------------------------------------------
rowTotal <- rowSums(smokeTab); rowTotal
colTotal <- colSums(smokeTab); colTotal
grandTotal <- sum(smokeTab); grandTotal
e <- outer(rowTotal, colTotal) / grandTotal; e
o <- smokeTab
stat <- sum ((e - o)^2 / e); stat
pval <- 1 - pchisq(stat, df = 2); pval

## ----family-smoking03, digits = 5----------------------------------------
chisq.test(smokeTab)

## ----family-smoking04----------------------------------------------------
attributes((chisq.test(smokeTab)))

## ----family-smoking05----------------------------------------------------
xchisq.test(smokeTab)

## ----family-smoking06, eval=FALSE, message = FALSE, opts.label = "figbig", cache = FALSE----
## vcd::mosaic( ~ student + parents,
##              data = FamilySmoking %>%
##                mutate(     # abbreviate labels to fit plot better
##                  student = c("NS", "S")[as.numeric(student)],
##                  parents = c("0", "1", "2")[as.numeric(parents)]
##                ),
##              shade = TRUE)

## ----family-smoking06-fig, echo=FALSE, opts.label = "figbig", cache = FALSE----
vcd::mosaic( ~ student + parents, 
             data = FamilySmoking %>%
               mutate(     # abbreviate labels to fit plot better
                 student = c("NS", "S")[as.numeric(student)],
                 parents = c("0", "1", "2")[as.numeric(parents)]
               ),
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

## ----smoking-bbs02-------------------------------------------------------
chisq.test(smTab2, simulate.p.value = TRUE, B = 5000)

## ----smoking-bbs03-------------------------------------------------------
smTab2[, -2]
chisq.test(smTab2[, -2])

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

## ----nfl-bt01, tidy = FALSE----------------------------------------------
NFL <- NFL2007 %>% mutate(
  dscore = homeScore - visitorScore,
  winner = ifelse(dscore > 0, home, visitor),
  loser  = ifelse(dscore > 0, visitor, home),
  homeTeamWon = dscore > 0
  )
head(NFL, 3)

## ----nfl-bt02, tidy = FALSE, message = FALSE-----------------------------
# fit Bradley-Terry model
require(BradleyTerry2)
NFL.model <- 
  BTm(cbind(homeTeamWon, !homeTeamWon), home, visitor, 
      data = NFL, id = "team") 

## ----nfl-bt03, tidy=FALSE------------------------------------------------
bta <- BTabilities(NFL.model)
nflRatings<- data.frame(
    team = rownames(bta),
    rating = bta[, "ability"],
    se = bta[, "s.e."],
    wins = as.vector(tally( ~ winner, data = NFL)),
    losses = as.vector(tally( ~ loser, data = NFL))
    )
row.names(nflRatings) <- NULL

nflRatings[rev(order(nflRatings$rating)), ]

## ----nfl-bt04------------------------------------------------------------
NFL <- NFL %>% 
  mutate(
    winnerRating = nflRatings$rating[as.numeric(winner)],
    loserRating  = nflRatings$rating[as.numeric(loser)], 
    upset = loserRating > winnerRating,
    pwinner = ilogit(winnerRating - loserRating))
# how big an upset was the Super Bowl?
NFL %>% tail(1)

## ----ncaa-bt01, tidy = FALSE---------------------------------------------
NCAA <- NCAAbb %>%  
  filter(season == "2009-10", !postseason) %>%
  mutate(
    neutralSite = grepl("n", notes, ignore.case = TRUE), # at neutral site?
    homeTeamWon = hscore > ascore)                     # did home team win?
# remove teams that didn't play > 5 at home and > 5 away
# (typically div II teams that played a few div I teams)
h <- tally( ~ home, data = NCAA); a <- tally( ~ away, data = NCAA)
deleteTeams <- c(names(h[h <= 5]), names(a[a <= 5]))
NCAA <- NCAA %>% 
  filter(!(home %in% deleteTeams | away %in% deleteTeams))

## ----ncaa-bt02, tidy = FALSE---------------------------------------------
# fit a Bradley-Terry model
require(BradleyTerry2) 
NCAA.model <- 
  BTm(cbind(homeTeamWon, 1 - homeTeamWon), home, away, data = NCAA)

## ----ncaa-bt03-----------------------------------------------------------
# look at top teams  
BTabilities(NCAA.model) %>% 
  as.data.frame() %>% 
  mutate(team = row.names(BTabilities(NCAA.model))) %>% 
  arrange(-ability) %>% head(6)

## ----ncaa-bt04-----------------------------------------------------------
require(BradleyTerry2)
# home team gets advantage unless on neutral court  
NCAA$homeTeam <- 
  data.frame(team = NCAA$home, at.home = 1 - NCAA$neutralSite)
NCAA$awayTeam <- data.frame(team = NCAA$away, at.home = 0)
NCAA.model2 <- 
  BTm(cbind(homeTeamWon, 1-homeTeamWon),
      homeTeam, awayTeam, id = "team", 
      formula = ~ team + at.home, data = NCAA)

## ----ncaa-bt05-----------------------------------------------------------
# the "order effect" is the coefficient on "at.home"  
coef(NCAA.model2)["at.home"] -> oe; oe
# expressed a multiplicative odds factor
exp(oe)
# prob home team wins if teams are "equal"
ilogit(oe)   

## ----ncaa-bt06, tidy=FALSE-----------------------------------------------
ab <-   
  BTabilities(NCAA.model2) 
ratings <- 
  ab[order(-ab[, "ability"]), ]
ratings[1:13, ]

## ----ncaa-bt07-----------------------------------------------------------
ratings[14:30, ]

## ----ncaa2010-bt08, tidy = FALSE-----------------------------------------
compareTeams <-
  function(team1, team2, model, 
           abilities = BTabilities(model)) { 
    a <- abilities[team1, 1]
    b <- abilities[team2, 1]
    return(ilogit(a - b))
} 
compareTeams("Kansas", "Kentucky", ab = ratings)
compareTeams("Butler", "Michigan St.", ab = ratings)
compareTeams("Butler", "Duke", ab = ratings)

## ----lady-bayes01, fig.keep = "none", warning = FALSE--------------------
gf_dist("beta", shape1 = 19, shape2 = 3, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(19, 3) posterior") %>%
  gf_lims(y = c(0, 6))
gf_dist("beta", shape1 = 10, shape2 = 2, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(10, 2) posterior") %>%
  gf_lims(y = c(0, 6))

## ----lady-bayes01-fig, echo = FALSE, warning = FALSE, results = "hide"----
gf_dist("beta", shape1 = 19, shape2 = 3, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(19, 3) posterior") %>%
  gf_lims(y = c(0, 6))
gf_dist("beta", shape1 = 10, shape2 = 2, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Beta(10, 2) posterior") %>%
  gf_lims(y = c(0, 6))

## ----lady-bayes02, fig.keep = "none", warning = FALSE--------------------
posterior_sample20 <- rbeta(1e5, shape1 = 19, shape2 = 3)
posterior_sample10 <- rbeta(1e5, shape1 = 10, shape2 = 2)
gf_dhistogram( ~ posterior_sample20, binwidth = 0.01) %>%
  gf_labs(title = "Sampling from a Beta(19, 3) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))
gf_dhistogram( ~ posterior_sample10, binwidth = 0.01, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Sampling from a Beta(10, 2) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))

## ----lady-bayes02-fig, echo = FALSE, warning = FALSE, results = "hide"----
posterior_sample20 <- rbeta(1e5, shape1 = 19, shape2 = 3)
posterior_sample10 <- rbeta(1e5, shape1 = 10, shape2 = 2)
gf_dhistogram( ~ posterior_sample20, binwidth = 0.01) %>%
  gf_labs(title = "Sampling from a Beta(19, 3) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))
gf_dhistogram( ~ posterior_sample10, binwidth = 0.01, xlim = c(0.4, 1)) %>%
  gf_labs(title = "Sampling from a Beta(10, 2) posterior") %>%
  gf_lims(x = c(0.4, 1), y = c(0,6))

## ----lady-bayes03, fig.keep = "none"-------------------------------------
1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 19, shape2 = 3, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(19,3)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))
1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 10, shape2 = 2, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(9,2)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))


## ----lady-bayes03-fig, echo = FALSE, results = "hide"--------------------
1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 19, shape2 = 3, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(19,3)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))
1 - xpbeta(c(0.5, 0.7, 0.9), shape1 = 10, shape2 = 2, 
           xlim = c(0.4, 1),
           refinements = list(
             labs(title = "Beta(9,2)"),
             scale_fill_brewer(type = "qual", palette = 3)
           ))


## ----lady-bayes04--------------------------------------------------------
# median of posterior
qbeta(0.5, shape1 = 19, shape2 = 3)
qbeta(0.5, shape1 = 10, shape2 = 2)

# median of posterior -- approxiamted by sampling
median(posterior_sample20)
median(posterior_sample10)

# mean of posterior -- approximated by sampling
mean(posterior_sample20)
mean(posterior_sample10)

# MAP
nlmax(function(x) dbeta(x, shape1 = 19, shape2 = 3), p = 0.5) %>% value()
nlmax(function(x) dbeta(x, shape1 = 10, shape2 = 2), p = 0.5) %>% value()

## ----lady-bayes05--------------------------------------------------------
# interval formed by removing equal tail probabilities
cdata( ~ posterior_sample20, 0.90)
cdata( ~ posterior_sample10, 0.90)
# HPDI
coda::HPDinterval(coda::as.mcmc(posterior_sample20), 0.90)
rethinking::HPDI(posterior_sample20, 0.90)
coda::HPDinterval(coda::as.mcmc(posterior_sample10), 0.90)
rethinking::HPDI(posterior_sample10, 0.90)

## ----lady-bayes06--------------------------------------------------------
binom.test(18, 20, conf.level = 0.90) %>% confint()
binom.test(9, 10, conf.level = 0.90) %>% confint()

## ----binom-bayes01-------------------------------------------------------
qbeta(c(0.025, 0.975), 38 + 1, 62 + 1)
binom.test(38, 100) %>% confint()            # for comparison
prop.test(38, 100) %>% confint()             # for comparison

## ----binom-bayes02-------------------------------------------------------
1- pbeta(0.5, 38 + 1, 62 + 1)      # 1-sided Bayesian p-value
binom.test(38, 100, alt = "less") %>% pval()      # for comparison
prop.test(38, 100, alt = "less") %>% pval()       # for comparison

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
# with very difuse prior, all the precision is coming from the data
# so sigma1 is sigma/sqrt(n)
5 / sqrt(length(x))

## ----binom-grid, fig.keep = "none"---------------------------------------
BinomGrid <- 
  expand.grid(pi = seq(0, 1, by = 0.001)) %>% 
  mutate(
    prior = 1,    # uniform prior
    likelihood = dbinom(20, size = 50, prob = pi),
    posterior = prior * likelihood                 # kernel of posterior
  ) 
posterior_sample <-
  with(BinomGrid, sample(pi, size = 1e5, prob = posterior, replace = TRUE))

# credible interval
cdata(~posterior_sample, 0.95)   # central 95% credible interval
# compare with analytical result above
gf_dhistogram( ~ posterior_sample, binwidth = 0.02, alpha = 0.4) %>%
  gf_dist("beta", shape1 = 21, shape2 = 31, color = "navy")
qbeta(c(0.025, 0.975), shape1 = 21, shape2 = 31)

## ----binom-grid-fig, fig.keep = "last", echo = FALSE, results = "hide"----
BinomGrid <- 
  expand.grid(pi = seq(0, 1, by = 0.001)) %>% 
  mutate(
    prior = 1,    # uniform prior
    likelihood = dbinom(20, size = 50, prob = pi),
    posterior = prior * likelihood                 # kernel of posterior
  ) 
posterior_sample <-
  with(BinomGrid, sample(pi, size = 1e5, prob = posterior, replace = TRUE))

# credible interval
cdata(~posterior_sample, 0.95)   # central 95% credible interval
# compare with analytical result above
gf_dhistogram( ~ posterior_sample, binwidth = 0.02, alpha = 0.4) %>%
  gf_dist("beta", shape1 = 21, shape2 = 31, color = "navy")
qbeta(c(0.025, 0.975), shape1 = 21, shape2 = 31)

## ----normal-grid01-------------------------------------------------------
x <- c(20, 24, 27, 28, 28, 28, 29, 30,  
       30, 30, 30, 32, 33, 34, 35, 38)
NormalGrid <- 
  expand.grid(
    mu = seq(20, 40, length.out = 200),
    sigma = seq(0.1, 15, length.out = 200)   # avoid sigma = 0 here
  ) %>%
  mutate(
    prior = dnorm(mu, 0, 20) * dgamma(sigma, shape = 3, rate = 1/3),
    likelihood = mapply(
      function(m, s) {prod(dnorm(x, mean = m, sd = s))}, 
      m = mu, s = sigma),
    posterior = prior * likelihood
  )
NormalGrid %>% 
  arrange(-posterior) %>%
  head(3)

## ----normal-grid02-------------------------------------------------------
PosteriorSample <-   
  sample(NormalGrid, size = 1e5, replace = TRUE, 
         prob = NormalGrid$posterior)

## ----normal-grid03, fig.keep = "none"------------------------------------
gf_histogram( ~ mu, data = PosteriorSample, binwidth = 0.2)
gf_histogram( ~ sigma, data = PosteriorSample, binwidth = 0.3)

## ----normal-grid04, fig.keep = "none"------------------------------------
gf_point(sigma ~ mu, data = PosteriorSample, 
         size = 0.4, alpha = 0.15) %>%
  gf_density2d()

## ----normal-grid04a, include = FALSE-------------------------------------
gf_hex(sigma ~ mu, data = PosteriorSample, 
       bins = 50) %>%
  gf_density2d(color = "white", alpha = 0.5) %>% 
  gf_refine(scale_fill_gradient2(midpoint = 400, low = "gray80", mid = "skyblue", high = "navy")) %>%
  gf_theme(legend.position = "top")

## ----normal-grid05-------------------------------------------------------
# 90% credible interval for the mean
cdata( ~ mu, data = PosteriorSample, p = 0.90) 
rethinking::HPDI(PosteriorSample$mu, 0.90)
# 90% CI for comparision
t.test(x, conf.level = 0.90) %>% confint()
# 90% credible interval for the standard deviation
cdata( ~ sigma, data = PosteriorSample, p = 0.90)
rethinking::HPDI(PosteriorSample$sigma, 0.90)

## ----normal-grid03-fig, echo = FALSE-------------------------------------
gf_histogram( ~ mu, data = PosteriorSample, binwidth = 0.2)
gf_histogram( ~ sigma, data = PosteriorSample, binwidth = 0.3)

## ----normal-grid04-fig, echo = FALSE-------------------------------------
gf_point(sigma ~ mu, data = PosteriorSample, 
         size = 0.4, alpha = 0.15) %>%
  gf_density2d()

## ----prior-compare, fig.keep = "none"------------------------------------
CoinGrid <- 
  expand.grid(
    pi = seq(0, 1, by = 0.001), 
    prior_name = 
      c("Unif(0, 1)", "Beta(20, 10)", "Tri(0.3, 0.7)", "Unif(0.3, 0.7)")) %>%
  mutate(
    prior = 
      case_when(
        prior_name == "Unif(0, 1)"     ~ dunif(pi, 0, 1), 
        prior_name == "Beta(20, 10)"   ~ dbeta(pi, 20, 10), 
        prior_name == "Tri(0.3, 0.7)"  ~ 
          triangle::dtriangle(pi, a = 0.3, b = 0.7, c = 0.5), 
        prior_name == "Unif(0.3, 0.7)" ~ dunif(pi, 0.3, 0.7)
      ),
    likelihood = dbinom(38, size = 100, prob = pi)) %>% 
  group_by(prior_name) %>%
  mutate(posterior =  prior * likelihood / sum(prior * likelihood) * 1000)

gf_line(prior ~ pi, color = ~ "prior", data = CoinGrid) %>%
  gf_line(posterior ~ pi, color = ~ "posterior", data = CoinGrid) %>%
  gf_facet_wrap( ~ prior_name) %>%
  gf_theme(legend.position = "top") %>% 
  gf_refine(guides(color = guide_legend("distribution: ")))

## ----prior-compare-fig, echo = FALSE, opts.label = "figbig"--------------
CoinGrid <- 
  expand.grid(
    pi = seq(0, 1, by = 0.001), 
    prior_name = 
      c("Unif(0, 1)", "Beta(20, 10)", "Tri(0.3, 0.7)", "Unif(0.3, 0.7)")) %>%
  mutate(
    prior = 
      case_when(
        prior_name == "Unif(0, 1)"     ~ dunif(pi, 0, 1), 
        prior_name == "Beta(20, 10)"   ~ dbeta(pi, 20, 10), 
        prior_name == "Tri(0.3, 0.7)"  ~ 
          triangle::dtriangle(pi, a = 0.3, b = 0.7, c = 0.5), 
        prior_name == "Unif(0.3, 0.7)" ~ dunif(pi, 0.3, 0.7)
      ),
    likelihood = dbinom(38, size = 100, prob = pi)) %>% 
  group_by(prior_name) %>%
  mutate(posterior =  prior * likelihood / sum(prior * likelihood) * 1000)

gf_line(prior ~ pi, color = ~ "prior", data = CoinGrid) %>%
  gf_line(posterior ~ pi, color = ~ "posterior", data = CoinGrid) %>%
  gf_facet_wrap( ~ prior_name) %>%
  gf_theme(legend.position = "top") %>% 
  gf_refine(guides(color = guide_legend("distribution: ")))

## ----google-map, eval = FALSE--------------------------------------------
## Positions <- rgeo(10)
## googleMap(position = Positions, mark = TRUE)

## ----dispersion01-sol----------------------------------------------------
val <- c(0,1,2,3,4)
frequency<- c(9,2,3,0,1)
n <- sum(frequency); n
x.bar <- sum(val * frequency) / n; x.bar
v <- sum(frequency * (val - x.bar)^2) / (n - 1); v
T <- 14 * v / x.bar; T
1- pchisq(T, 14)

## ----dispersion02-sol, seed = 12345--------------------------------------
T <- function(x) c(T = var(x) / mean(x))
# one-sided p-values
Sims1 <- (do(10000) * T(rpois(15, lambda = 1))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims5 <- (do(10000) * T(rpois(15, lambda = 5))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
Sims50 <- (do(10000) * T(rpois(15, lambda = 50))) %>% mutate(p.val = 1 - pchisq(14 * T, df = 14))
# It isn't necessary to mulitiply T by (n-1) to assess linearity in a qq-plot
gf_qq(~ T, data = Sims1, distribution = qchisq, dparams = list(df = 14), geom = "line") %>%
  gf_labs(title = "lambda = 1")
gf_qq(~ T, data = Sims5, distribution = qchisq, dparams = list(df = 14), geom = "line") %>%
  gf_labs(title = "lambda = 5")
gf_qq(~ T, data = Sims50, distribution = qchisq, dparams = list(df = 14), geom = "line") %>%
  gf_labs(title = "lambda = 50")

# now we compare p-values to uniform distribution, zooming in on small p-values
gf_qq( ~ p.val, data = Sims1, distribution = qunif, geom = "line") %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.5, color = "red") %>%
  gf_labs(title = "lambda = 1") %>%
  gf_lims(x = c(0, 0.1), y = c(0, 0.1))
gf_qq( ~ p.val, data = Sims5, distribution = qunif, geom = "line") %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.5, color = "red") %>%
  gf_labs(title  = "lambda = 5") %>%
  gf_lims(x = c(0, 0.1), y = c(0, 0.1))
gf_qq( ~ p.val, data = Sims50, distribution = qunif, geom = "line") %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.5, color = "red") %>%
  gf_labs(title  = "lambda = 50") %>% 
  gf_lims(x = c(0, 0.1), y = c(0, 0.1))


## ----LinearModels, child="LinearModels.Rnw", eval=includeChapter[6]------

## ----LM-setup, include = FALSE, cache = FALSE----------------------------
require(fastR2)
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
## cholesterol ~ age + I(age^2)   # see below for explanation of I()
## cholesterol ~ poly(age,2)      # 2nd degree polynomial in age

## ----eval = FALSE--------------------------------------------------------
## gpa ~ SATM + SATV
## gpa ~ I(SATM + SATV)

## ----small-data01, fig.keep = "none"-------------------------------------
SmallData <- data.frame(x = c(1, 2, 3, 4), y = c(2, 5, 6, 8))
gf_point(y ~ x, data = SmallData)

## ----small-data01-fig, echo = FALSE--------------------------------------
SmallData <- data.frame(x = c(1, 2, 3, 4), y = c(2, 5, 6, 8))
gf_point(y ~ x, data = SmallData)

## ----small-data02, fig.keep = "none"-------------------------------------
model <- lm(y ~ x , data = SmallData) 
gf_lm(y ~ x, data = SmallData) %>%
  gf_point()

## ----small-data02-fig, echo = FALSE--------------------------------------
model <- lm(y ~ x , data = SmallData) 
gf_lm(y ~ x, data = SmallData) %>%
  gf_point()

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
# set up the v and u vectors
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

## ----trebuchet01, fig.show="hide"----------------------------------------
treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
coef(treb.model)
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm()

## ----trebuchet01-fig, echo=FALSE, results = "hide"-----------------------
treb.model <- lm(distance ~ projectileWt, data = Trebuchet2) 
coef(treb.model)
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm()

## ----trebuchet-sol, fig.keep = "last"------------------------------------
idx <- which.min(resid(treb.model))
lm(distance ~ projectileWt, data = Trebuchet2[-idx, ]) %>%
     coef()
treb.model %>% coef()
gf_point(distance ~ projectileWt, data = Trebuchet2[-idx, ]) %>%
  gf_lm() %>%
  gf_lm(data = Trebuchet2, color = "red") 

## ----elasticband01, fig.show = "hide"------------------------------------
data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband) 
coef(eband.model)
gf_lm(distance ~ stretch, data = elasticband) %>%
  gf_point()

## ----elasticband01-fig, results = "hide", echo = FALSE-------------------
data(elasticband, package = "DAAG") 
eband.model <- lm(distance ~ stretch, data = elasticband) 
coef(eband.model)
gf_lm(distance ~ stretch, data = elasticband) %>%
  gf_point()

## ----regression01, tidy = FALSE, fig.keep = "none"-----------------------
GaltonBoys <- 
  Galton %>%
  filter(sex == "M") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup()
Galton.lm <- lm(height ~ father, data = GaltonBoys) 
coef(Galton.lm)
gf_lm(height ~ father, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4)

## ----regression01-fig, echo = FALSE--------------------------------------
GaltonBoys <- 
  Galton %>%
  filter(sex == "M") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup()
Galton.lm <- lm(height ~ father, data = GaltonBoys) 
coef(Galton.lm)
gf_lm(height ~ father, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4)

## ----regression02--------------------------------------------------------
favstats( ~ height, data = GaltonBoys)
favstats( ~ father, data = GaltonBoys)

## ----regression03--------------------------------------------------------
predictedHeight <- makeFun(Galton.lm) 
predictedHeight(father = 75)
predictedHeight(father = 65)

## ----regression04, tidy = FALSE------------------------------------------
GaltonBoys <-
  GaltonBoys %>%
  mutate(midparent = (father + mother) / 2)
favstats( ~ height, data = GaltonBoys)
favstats( ~ midparent, data = GaltonBoys)

## ----regression05, tidy = FALSE, fig.keep = "none"-----------------------
GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
gf_lm(zheight ~ zmidparent, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4) %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.4)

## ----regression05-fig, echo = FALSE, results = "hide"--------------------
GaltonBoys <- 
  GaltonBoys %>%
  mutate(zheight = zscore(height),
         zmidparent = zscore(midparent)
  )
Galtonz.lm <- lm(zheight ~ zmidparent, data = GaltonBoys) 
coef(Galtonz.lm)
gf_lm(zheight ~ zmidparent, data = GaltonBoys) %>%
  gf_jitter(alpha = 0.4) %>%
  gf_abline(slope = 1, intercept = 0, alpha = 0.4)

## ----bands-sol-----------------------------------------------------------
model1 <- lm(distance ~ stretch, data = elasticband)
model2 <- lm(distance ~ stretch, data = RubberBand)
msummary(model1)
msummary(model2)

## ----trebuchet02, tidy = FALSE-------------------------------------------
treb.model <- 
  lm(distance ~ projectileWt, data = Trebuchet2)
msummary(treb.model)   # terser output than summary() produces

## ----trebuchet03---------------------------------------------------------
-0.0946 + c(-1, 1) * 0.01713 * qt(0.975, df = 14)  # CI by hand
confint(treb.model, "projectileWt")             # CI using confint()

## ----confint-lm----------------------------------------------------------
stats:::confint.lm

## ----elasticband02-------------------------------------------------------
msummary(eband.model)
4.554 + c(-1, 1) * 1.543 * qt(0.975, df = 5)          # CI by hand
confint(eband.model, "stretch")                   # CI using confint()

## ----anova-trebuchet-----------------------------------------------------
treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
anova(treb.model)

## ----trebuchet04, tidy = FALSE-------------------------------------------
rsquared(treb.model)

## ----eval = FALSE--------------------------------------------------------
## lm(y ~  0 + x)
## lm(y ~ -1 + x)

## ----anova-elasticband---------------------------------------------------
anova(eband.model)

## ----trebuchet05, tidy = FALSE-------------------------------------------
treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
treb.dist <- makeFun(treb.model)
treb.dist(projectileWt = 44)
treb.dist(projectileWt = 44, interval = "confidence")
treb.dist(projectileWt = 44, interval = "prediction")

## ----trebuchet06, fig.keep = "none", tidy = FALSE, warning = FALSE-------
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm() %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, fill = "skyblue",
    data = cbind(Trebuchet2, predict(treb.model, interval = "prediction"))
    ) %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, 
    data = cbind(Trebuchet2, predict(treb.model, interval = "confidence"))) 
# simpler way, using gf_lm()
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>%
  gf_lm(interval = "confidence")

## ----trebuchet06-fig, results = "hide", echo = FALSE, tidy = FALSE, fig.keep = "last", warning = FALSE----
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm() %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, fill = "skyblue",
    data = cbind(Trebuchet2, predict(treb.model, interval = "prediction"))
    ) %>%
  gf_ribbon(
    lwr + upr ~ projectileWt, 
    data = cbind(Trebuchet2, predict(treb.model, interval = "confidence"))) 
# simpler way, using gf_lm()
gf_point(distance ~ projectileWt, data = Trebuchet2) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>%
  gf_lm(interval = "confidence")

## ----elasticband03, tidy = FALSE-----------------------------------------
eband.dist <- makeFun(eband.model)
eband.dist(stretch = 30)
eband.dist(stretch = 30, interval = "confidence")
eband.dist(stretch = 30, interval = "prediction")

## ----eband-fig, results = "hide", echo = FALSE, fig.keep = "last", warning = FALSE----
gf_point(distance ~ stretch, data = elasticband) %>%
  gf_lm() %>%
  gf_ribbon(lwr + upr ~ stretch, fill = "skyblue",
            data = cbind(elasticband, predict(eband.model, interval = "prediction"))) %>%
  gf_ribbon(lwr + upr ~ stretch, 
            data = cbind(elasticband, predict(eband.model, interval = "confidence"))) 
gf_point(distance ~ stretch, data = elasticband) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>% # prediction band the easy way
  gf_lm(interval = "confidence")     # confidence band the easy way

## ----vcov----------------------------------------------------------------
treb.model <-  
  lm(distance ~ projectileWt, data = Trebuchet2)
vcov(treb.model) 
sqrt(diag(vcov(treb.model)))
treb.model %>% summary() %>% coef()

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
gf_point(stresidual ~ fit, data = rdata) %>%
  gf_facet_wrap(~ group, scale = "free") %>%
  gf_labs(y = "residual") %>%
  gf_theme(axis.ticks = element_blank(), axis.text = element_blank())
                    # ylim = c(-1.1, 1.1) * max(abs(rdata$stresidual)),
                    # as.table = T)

## ----rstandard-----------------------------------------------------------
SmallData <- data.frame(x = c(1, 2, 3, 4), y = c(2, 5, 6, 8))
small.model <- lm(y ~ x, data = SmallData)
# standardized residual computing manually
e <- resid(small.model); e
sigma.hat <- sqrt(sum(e^2) / 2);   sigma.hat       # internal estimate
h <- hatvalues(small.model); h
e / (sigma.hat * sqrt(1 - h))
# standardized residuals using rstandard()
rstandard(small.model)

## ----hatvalues-----------------------------------------------------------
h <- hatvalues(small.model); h
range(h)   # should be contained in [0,1]
sum(h)     # should be p = 2
n <- nrow(SmallData)
with(SmallData, 1/n + (x - mean(x))^2/sum((x - mean(x))^2))

## ----rstudent------------------------------------------------------------
rstudent(small.model)
# computing the first externally studentized residual by hand
smaller.model <- lm(y ~ x, data = SmallData[-1, ])
# modified sigma.hat based on smaller model
summary(smaller.model)$sigma
sigma.hat <- sqrt(sum(resid(smaller.model)^2) / 1); sigma.hat  
# rescaling the 1st residual
e[1] / (sigma.hat * sqrt(1 - h[1]))        

## ----star01, tidy = FALSE, fig.keep = "none", message = FALSE------------
Stars <- faraway::star
star.plot1 <- gf_point(light ~ temp, data = Stars)
# select all but 4 coolest stars
HotStars <- Stars %>% filter(temp > 3.7)   
star.model1 <- lm(light ~ temp, data = Stars)
star.model2 <- lm(light ~ temp, data = HotStars)
gf_point(light ~ temp, data = Stars) %>%
  gf_lm(color = "gray50", linetype = "dotted") %>%
  gf_lm(color = "red", linetype = "dashed", data = HotStars) %>%
  gf_text(light ~ (temp + 0.04), label = ~ as.character(id),
          data = Stars %>% mutate(id = 1:nrow(.)) %>% filter(temp < 4.0))

## ----star-fig, results = "hide", echo = FALSE, message = FALSE-----------
Stars <- faraway::star
star.plot1 <- gf_point(light ~ temp, data = Stars)
# select all but 4 coolest stars
HotStars <- Stars %>% filter(temp > 3.7)   
star.model1 <- lm(light ~ temp, data = Stars)
star.model2 <- lm(light ~ temp, data = HotStars)
gf_point(light ~ temp, data = Stars) %>%
  gf_lm(color = "gray50", linetype = "dotted") %>%
  gf_lm(color = "red", linetype = "dashed", data = HotStars) %>%
  gf_text(light ~ (temp + 0.04), label = ~ as.character(id),
          data = Stars %>% mutate(id = 1:nrow(.)) %>% filter(temp < 4.0))

## ----star-fig01, results = "hide", echo = FALSE--------------------------
plot(star.model1, which = c(1:2, 4:5))

## ----star-fig02, results = "hide", echo = FALSE--------------------------
plot(star.model2, which = c(1:2, 4:5))

## ----plot-starmodels, fig.keep = "none"----------------------------------
plot(star.model1, which = c(1:2, 4:5))
plot(star.model2, which = c(1:2, 4:5))

## ----star-dfbeta, fig.show = "hide", tidy = FALSE------------------------
HotStars <- HotStars %>% 
  mutate(
    dfbeta_temp = dfbeta(star.model2)[, "temp"]
  )
gf_point(dfbeta_temp ~ index, data = HotStars) %>%
  gf_labs(y = "DFBETA") %>%
  gf_text(dfbeta_temp ~ (1.5 + index), 
          data = HotStars %>% filter(abs(dfbeta_temp) > 0.5),
          label = ~ as.character(index))
coef(lm(light ~ temp, HotStars))
coef(lm(light ~ temp, HotStars[-7, ]))

## ----star-dfbeta-fig, echo = FALSE, results = "hide"---------------------
HotStars <- HotStars %>% 
  mutate(
    dfbeta_temp = dfbeta(star.model2)[, "temp"]
  )
gf_point(dfbeta_temp ~ index, data = HotStars) %>%
  gf_labs(y = "DFBETA") %>%
  gf_text(dfbeta_temp ~ (1.5 + index), 
          data = HotStars %>% filter(abs(dfbeta_temp) > 0.5),
          label = ~ as.character(index))
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
gf_point(y ~ x)

## ----tukey-buldge-many-fig, results = "hide", echo = FALSE, opts.label = "figbig"----
ddd <- ddd %>% 
  mutate(
    afacet = factor(paste("a=", a, sep = "")),
    bfacet0 = factor(paste("b=", b, sep = "")),
    bfacet = factor(bfacet0, levels = rev(levels(bfacet0)))
  )
         
gf_point(Y ~ X, data = ddd, color = ~original) %>%
  gf_facet_grid(bfacet ~ afacet, scale = "free") %>%
  gf_theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none") 

## ----balldrop, fig.show = "hide"-----------------------------------------
ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
gf_lm(time ~ height, data = BallDrop) %>%
  gf_point()
plot(ball.model, w = 1)

## ----balldrop-fig, results = "hide", echo = FALSE------------------------
ball.model <- lm(time ~ height, BallDrop)
msummary(ball.model)
gf_lm(time ~ height, data = BallDrop) %>%
  gf_point()
plot(ball.model, w = 1)

## ----balldrop-trans, fig.show = "hide"-----------------------------------
ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
gf_point(time ~ height, data = BallDrop) %>%
  gf_lm(formula = y ~ sqrt(x), interval = "prediction", 
             fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelT, w = 1)

## ----balldrop-trans-fig, results = "hide", echo = FALSE------------------
ball.modelT <- lm(time ~ sqrt(height), data = BallDrop) 
msummary(ball.modelT)
gf_point(time ~ height, data = BallDrop) %>%
  gf_lm(formula = y ~ sqrt(x), interval = "prediction", 
             fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelT, w = 1)

## ----balldrop-sol--------------------------------------------------------
lm(time^2 ~ height, data = BallDrop)
lm(log(time) ~ log(height), BallDrop)

## ----balldrop-avg, fig.show = "hide", tidy = FALSE-----------------------
BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
msummary(ball.modelA)
gf_point(time ~ height, data = BallDropAvg) %>%
  gf_lm(formula = y ~ sqrt(x), fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelA, w = 1)

## ----balldrop-avg-fig, results = "hide", echo = FALSE--------------------
BallDropAvg <-  
  BallDrop %>% 
  group_by(height) %>%
  summarise(time = mean(time))
BallDropAvg
ball.modelA <- lm(time ~ sqrt(height), data = BallDropAvg)
msummary(ball.modelA)
gf_point(time ~ height, data = BallDropAvg) %>%
  gf_lm(formula = y ~ sqrt(x), fill = "skyblue") %>%
  gf_lm(formula = y ~ sqrt(x), interval = "confidence") 
plot(ball.modelA, w = 1)

## ----soap01--------------------------------------------------------------
Soap.model1 <- lm(weight ~ day, data = Soap)
msummary(Soap.model1)

## ----soap02--------------------------------------------------------------
Soap.model2 <- lm(I(weight^(1/3)) ~ day, data = Soap)
msummary(Soap.model2)

## ----soap-fig, results = "hide", echo = FALSE----------------------------
fit1 <- makeFun(Soap.model1)
# transformation required to undo transformation on y
fit2 <- makeFun(Soap.model2, transformation = function(x) x^3)
gf_point(weight ~ day, data = Soap) %>%
  gf_function(fun = fit1, color = "navy") %>%
  gf_function(fun = fit2, color = "red") 

## ----soap03--------------------------------------------------------------
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
gf_point(period ~ length, data = Pendulum) %>%
  gf_function(fun = f)

## ----pendulum-sol03, warning = FALSE-------------------------------------
model2 <- lm(log(period) ~ log(length), data = Pendulum)
msummary(model2)
g <- makeFun(model2) 
plot(model2, w = 1)
plot(model2, w = 2)

## ----pendulum-sol04, fig.keep = "last", warning = FALSE------------------
gf_point(period ~ length, data = Pendulum) %>%
  gf_fun(f(l) ~ l) %>%
  gf_fun(exp(g(l)) ~ l, col = "red")

## ----pendulum-sol05------------------------------------------------------
confint(model2)

## ----cornnit-sol01-------------------------------------------------------
data(cornnit, package = "faraway")
cornnit.mod <- lm(yield ~ log(1 + nitrogen), data = cornnit)
msummary(cornnit.mod)
fit <- makeFun(cornnit.mod)
gf_point(yield ~ log(1 + nitrogen), data = cornnit) %>%
  gf_lm() 
gf_point(yield ~ nitrogen, data = cornnit) %>%
  gf_function(fun = fit)
gf_point(yield ~ nitrogen, data = cornnit) %>%
  gf_lm(formula = y ~ log(1 + x)) 
plot(cornnit.mod, w = 1:3)

## ----cornnit-sol02-------------------------------------------------------
cornnit.mod2 <- lm(yield ~ log(1 + nitrogen), data = cornnit[-21, ])
msummary(cornnit.mod2)
fit2 <- makeFun(cornnit.mod2)
gf_point(yield ~ log(1 + nitrogen), data = cornnit[-21, ]) %>%
  gf_lm()
gf_point(yield ~ nitrogen, data = cornnit[-21, ]) %>%
  gf_function(fun = fit2)
gf_point(yield ~ nitrogen, data = cornnit[-21, ]) %>%
  gf_lm(formula = y ~ log(1 + x)) %>%
  gf_lm(formula = y ~ log(1 + x), data = cornnit, color = "gray50") 
plot(cornnit.mod2, w = 1:3)

## ----cornnit-sol03-------------------------------------------------------
cornnit.mod3 <- lm(yield^2 ~ sqrt(nitrogen), data = cornnit)
fit3 <- makeFun(cornnit.mod3, transformation = sqrt)
msummary(cornnit.mod3)
gf_point(yield^2 ~ sqrt(nitrogen), data = cornnit) %>%
  gf_lm()
gf_point(yield ~ nitrogen, data = cornnit) %>%
  gf_function(fit3)
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

## ----trebuchet07---------------------------------------------------------
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
t.test(~ ACT, data = ACTgpa)
t.test(~ GPA, data = ACTgpa)
grades.model <- lm(GPA ~ ACT, data = ACTgpa)
msummary(grades.model)
gf_point(GPA ~ ACT, data = ACTgpa) %>%
  gf_lm(interval = "prediction", fill = "skyblue") %>%
  gf_lm(interval = "confidence") %>%
  gf_lm()
act2gpa <- makeFun(grades.model)
act2gpa(ACT = 25, interval = "confidence")
act2gpa(ACT = 30, interval = "prediction")

## ----drag-sol------------------------------------------------------------
model1 <- lm(velocity^2 ~ force.drag, data = Drag)
model2 <- lm(velocity ~ sqrt(force.drag), data = Drag)
model3 <- lm(log(velocity) ~ log(force.drag), data = Drag)
msummary(model1)
msummary(model2)
msummary(model3)

## ----drag-fig, results = "hide", echo = FALSE----------------------------
gf_point(velocity^2 ~ force.drag, data= Drag, color = ~ factor(height))
plot(model1, w = 1)
gf_point(velocity ~ force.drag, data = Drag,  color = ~ factor(height)) %>%
  gf_refine(scale_x_log10(), scale_y_log10()) 
plot(model3, w = 1)

## ----spheres-sol01-------------------------------------------------------
gf_point(log(mass) ~ log(diameter), data = Spheres)
spheres.lm <- lm(log(mass) ~ log(diameter), data = Spheres)
confint(spheres.lm)
plot(spheres.lm, w = 1:2)

## ----spheres-sol02, fig.keep = "last"------------------------------------
mass <- makeFun(spheres.lm) 
gf_point(mass ~ diameter, data = Spheres) %>%
  gf_function(fun = mass, alpha = 0.5)

## ----spheres-sol03-------------------------------------------------------
confint(spheres.lm, level = .96)

## ----taste01-------------------------------------------------------------
favstats(score ~ scr, data = TasteTest)

## ----taste02-------------------------------------------------------------
taste.model <- lm(score ~ scr, data = TasteTest) 
msummary(taste.model)

## ----taste03-------------------------------------------------------------
confint(taste.model)

## ----taste04-------------------------------------------------------------
confint(taste.model, "scrfine") / 50  

## ----corn01, tidy = FALSE, messag = FALSE--------------------------------
# the Corn data frame has an inconvenient "shape" 
# (each type of Corn is in its own column)
head(Corn, 3)                                   
require(tidyr)
# this puts all the yields in one column and type of seed in another
Corn2 <- Corn %>% gather(key = "treatment", value = "yield")
# inspect a few rows in each treatment group
Corn2 %>% group_by(treatment) %>% do(head(., 3))

## ----corn02--------------------------------------------------------------
favstats(yield ~ treatment, data = Corn2)
Corn.model <- lm(yield ~ treatment, data = Corn2)
msummary(Corn.model)

## ----paired-corn-sol-----------------------------------------------------
t.test(~ (reg-kiln), data = Corn)    # paired
t.test(Corn$reg, Corn$kiln)        # 2-sample

## ----tirewear-sol01------------------------------------------------------
msummary(lm(weight ~ groove, data = TireWear))
gf_lm(weight ~ groove, data = TireWear) %>%
  gf_point()

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

## ----taste05-------------------------------------------------------------
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
gf_line(pow(effect) ~ effect) %>%
  gf_labs(y = "power", x = "effect size", 
          title = "Power of a 2-sample test (n = 50)")

## ----power-t-test05-fig, results = "hide", echo = FALSE------------------
pow <- function(effect) {
    power.t.test(delta = effect, n = 50)$power
}
effect = seq(0, 2, by = 0.05)
gf_line(pow(effect) ~ effect) %>%
  gf_labs(y = "power", x = "effect size", 
          title = "Power of a 2-sample test (n = 50)")

## ----power-t-test06------------------------------------------------------
power.t.test(delta = 0.5, power = 0.8, type = "one.sample")

## ----power-t-test07------------------------------------------------------
power.t.test(delta = 0.5, power = 0.8, type = "paired")

## ------------------------------------------------------------------------
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05)
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, 
             type = "one.sample")

## ----power-sims-sol------------------------------------------------------
Sims2 <- do(5000) * t.test(rnorm(30, 0.5, 1), rnorm(30, 0, 1)) %>% pval()
prop( ~(p.value <= 0.05), data = Sims2)
Sims1 <- do(5000) * t.test(rnorm(30, 0.5, 1)) %>% pval()
prop( ~(p.value <= 0.05), data = Sims1)

## ----power-slr-sol-------------------------------------------------------
x <- seq(0, 10, length.out = 24)
Sims <- do(1000) * {
  lm(x + rnorm(24, 0, 4) ~ x) %>% 
    summary() %>% 
    coef() %>% 
    (function(x) x[2,4])    # p-value for slope
}
prop( ~(result <= 0.01), data = Sims)  # power

## ----orings01, tidy = FALSE----------------------------------------------
# select the version of this data set in the faraway package
data(orings, package = "faraway")        
orings <-
  orings %>% mutate(failure = damage != 0)   # convert to binary response
orings.model <- 
    glm(failure ~ temp, data = orings, family = binomial(link = logit))
msummary(orings.model)

## ----orings-fig, results = "hide", echo = FALSE--------------------------
pred_damage <- makeFun(orings.model)
Pred_data <- 
  data_frame(
    temp = seq(30, 100, by = 2),
    pred = pred_damage(temp))
gf_point(damage / 6 ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred ~ temp, data = Pred_data) %>%
  gf_lims(x = c(30, 100), y = c(-0.05, 1.05)) %>%
  gf_labs(y = "proportion of O-rings damaged")

orings <- orings %>% mutate(fail = as.numeric(failure))
gf_point(fail ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred ~ temp, data = Pred_data) %>%
  gf_lims(x = c(30, 100), y = c(-0.05, 1.05)) %>%
  gf_labs(y = "O-ring failure (1 = yes, 0 = no)")

## ----orings02, tidy = FALSE, digits = 5----------------------------------
# by default, predict() works on the linear model scale
r <- predict(orings.model, newdata = data.frame(temp = 31)); r
ilogit(r)
# but we can ask for it to work on the "response" scale
predict(orings.model, newdata = data.frame(temp = 31), type = "response")

## ----orings03, digits = 5------------------------------------------------
# by default, makeFun() uses type = "response" and 
# returns values on data scale
temp2damage <- makeFun(orings.model) 
temp2damage(temp = 31) 
makeFun(orings.model)(31)   # We can do it all in one line if we prefer
# the other option is type = "link"
temp2damage <- makeFun(orings.model, type = "link")
temp2damage(temp = 31)

## ----orings04, digits = 5------------------------------------------------
p <- makeFun(orings.model)(31)
1 - (1-p)^(1/6) -> q; q       # P(damage to particular O-ring)
1 - dbinom(0, 6, q)           # P(damage to >0 O-rings)
cbind(0:6, dbinom(0:6, 6, q)) # table of all probabilities

## ----orings05, digits = 5------------------------------------------------
orings.model2 <-                  # link = logit is default, so unnecessary
    glm(cbind(damage, 6 - damage) ~ temp, data = orings, 
        family = binomial(link = logit))
msummary(orings.model2)
p1 <- predict(orings.model, newdata = data.frame(temp = 31), type = "response"); p1
p2 <- predict(orings.model2, newdata = data.frame(temp = 31), type = "response"); p2
dbinom(0, 6, prob = p2)               # 0 damaged O-rings
pred_damage1 <- makeFun(orings.model)
pred_damage2 <- makeFun(orings.model2)
Pred_data <- 
  data_frame(
    temp = seq(30, 100, by = 2),
    pred1 = pred_damage1(temp),
    pred2 = pred_damage2(temp)
    )
gf_point(damage / 6 ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred1 ~ temp, data = Pred_data, color = "gray50") %>%
  gf_line(pred2 ~ temp, data = Pred_data, color = "blue") %>%
  gf_labs(y = "proportion of O-rings damaged") 

## ----runswins01----------------------------------------------------------
head(MLB2004, 4)

## ----runswins02, tidy = FALSE--------------------------------------------
BB <- MLB2004 %>% 
  mutate(runmargin = (R - OR) / G)

# data frame has summarized data for each team, so different syntax here:
bb.glm <- glm(cbind(W, L) ~ runmargin, data = BB, family = "binomial") 
msummary(bb.glm)

## ----runswins03, fig.keep = "none", tidy = FALSE-------------------------
BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(bb.glm)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

gf_point(winP ~ predWinP, data = BB) %>%
  gf_abline(slope = 1, intercept = 0)

## ----runswins04, fig.keep = "none"---------------------------------------
Aux_data <- 
  data_frame(
   runmargin = seq(-3.5, 3.5, by = 0.1),
   winP = makeFun(bb.glm)(runmargin = runmargin)
  )
gf_point(winP ~ runmargin, data = BB) %>%
  gf_line(winP ~ runmargin, data = Aux_data, alpha = 0.4)

## ----runswins03-fig, results = "hide", echo = FALSE----------------------
BB <- 
  BB %>% 
  mutate( 
    winP = W / G, 
    predWinP = makeFun(bb.glm)(runmargin), 
    winPdiff = winP - predWinP
    ) 
BB %>% arrange(-abs(winPdiff)) %>% select(1, 22:24) %>% head()

gf_point(winP ~ predWinP, data = BB) %>%
  gf_abline(slope = 1, intercept = 0)
Aux_data <- 
  data_frame(
   runmargin = seq(-3.5, 3.5, by = 0.1),
   winP = makeFun(bb.glm)(runmargin = runmargin)
  )
gf_point(winP ~ runmargin, data = BB) %>%
  gf_line(winP ~ runmargin, data = Aux_data, alpha = 0.4)

## ----orings-ci01---------------------------------------------------------
st.err <- sqrt(diag(vcov(orings.model))); st.err
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
bb.glm <-
  glm(cbind(W, L) ~ runmargin, data = BB, family = binomial()) 
bb.lm <- lm(winP ~ runmargin, data = BB) 
msummary(bb.lm)
BB <- BB %>% 
  mutate(
    glmPredWinP = makeFun(bb.glm)(runmargin = runmargin), 
    lmPredWinP  = makeFun(bb.lm) (runmargin = runmargin)
    )
plot(bb.lm, w = 2)
plot(bb.glm, w = 2)
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
gf_point(propDead ~ conc, data = tbl) %>%
  gf_labs(y = "predicted death rate", x = "concentration of glyphosate") %>%
  gf_function(fun = dead, alpha = 0.5)

## ----include = FALSE, eval = FALSE---------------------------------------
## tbl2 <- Buckthorn %>%
##   group_by(conc) %>%
##   summarise(
##     propDead = mean(dead)
##     )
## tbl2
## concentrations = seq(0, 0.5, by = 0.02)
## fits <- predict(buck.model, new = data.frame(conc = concentrations),
##                 type = "response")

## ----buckthorn-sol05-----------------------------------------------------
observed <- tbl[, 3:4]; observed 
expected <- tbl[, 6:7]; expected
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson
# pvals
1 - pchisq(pearson, df = 2)
1 - pchisq(lrt, df = 2)

## ----logit-probit-sol, fig.keep = "last"---------------------------------
gf_fun(ilogit(3 + 2 * x) ~ x, xlim = c(-6, 3), 
       size = 1.0, col = "gray70") %>%
gf_fun(pnorm((1.5 + x) * sqrt(2 * pi)/2) ~ x, col = "red")

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
gf_fun(f.logit(r) ~ r, xlim = c(-2, 2)) %>%
gf_fun(f.probit(r) ~ r, size = 2, color = "red", alpha = 0.4)

## ----orings-sol, fig.keep = "last", tidy = FALSE-------------------------
orings.logit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = logit))
orings.probit <- 
  glm(failure ~ temp, data = orings, family = binomial(link = probit))
confint(orings.logit)
confint(orings.probit)
g.logit <- makeFun(orings.logit) 
g.probit <- makeFun(orings.probit) 
gf_fun(g.logit(t) ~ t, xlim = c(25, 90)) %>%
gf_fun(g.probit(t) ~ t, lty = 2, lwd = 1.5, alpha = .4, col = "red")

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
gf_point(propDead ~ conc, data = tbl2) %>%
  gf_labs(y = "predicted death rate", x = "concentration of glyphosate") %>%
  gf_fun(dead2(c) ~ c, linetype = "dotted") %>%
  gf_fun(dead(c) ~ c, linetype = "dashed")

## ----bucktorn-probit-sol03, tidy = FALSE---------------------------------
observed <- tbl2[ , 3:4]
expected <- tbl2[ , 6:7]
lrt <- 2 * sum(observed *  log (observed / expected)); lrt
pearson <- sum((observed - expected)^2 / expected); pearson

## ------------------------------------------------------------------------
# pvals
1 - pchisq(pearson, df = 2)
1 - pchisq(lrt, df = 2)

## ----deviance01----------------------------------------------------------
null.glm <- 
  glm(cbind(W, L) ~ 1, data = BB, family = "binomial") 
bb.glm <- 
  glm(cbind(W, L) ~ runmargin, data = BB, family = "binomial") 
saturated.glm <- 
  glm(cbind(W, L) ~ factor(1:30), data = BB, family = "binomial") 

## ----deviance02, digits = 4----------------------------------------------
msummary(bb.glm)
deviance(bb.glm)
deviance(null.glm)
deviance(saturated.glm) %>% round(10)

## ----deviance03----------------------------------------------------------
deviance(null.glm) - deviance(bb.glm)
1 - pchisq(deviance(null.glm) - deviance(bb.glm), df = 1)

## ----deviance04----------------------------------------------------------
deviance(bb.glm) - deviance(saturated.glm)
1 - pchisq(deviance(bb.glm) - deviance(saturated.glm), df = 28)

## ----deviance05----------------------------------------------------------
glm(cbind(W, L) ~ SLG, data = BB, family = "binomial") %>%
  msummary()

## ----students01, fig.keep = "none", warning = FALSE----------------------
act.glm <- glm(grad ~ ACT, data = Students, family = "binomial")
fit1 <- makeFun(act.glm)
gf_point(prop.grad ~ ACT.decile, 
         data = Students %>% 
           mutate(ACT.decile = ntiles(ACT, 10, format = "mean")) %>%
           group_by(ACT.decile) %>% 
           summarise(prop.grad = prop(grad))
) %>% 
  gf_fun(fit1(act) ~ act)

## ----students02----------------------------------------------------------
msummary(act.glm)

## ----students03, fig.keep = "none", warning = FALSE----------------------
sat.glm <- glm(grad ~ SAT, data = Students, family = "binomial")
fit2 <- makeFun(sat.glm)
gf_point(
  prop.grad ~ SAT.decile,
  data = Students %>% 
    mutate(SAT.decile = ntiles(SAT, 10, format = "mean")) %>%
    group_by(SAT.decile) %>% 
    summarise(prop.grad = prop(grad))
) %>%
  gf_fun(fit2(sat) ~ sat)

## ----students04----------------------------------------------------------
msummary(sat.glm)

## ----students-fig, echo = FALSE, results = "hide", warning = FALSE-------
act.glm <- glm(grad ~ ACT, data = Students, family = "binomial")
fit1 <- makeFun(act.glm)
gf_point(prop.grad ~ ACT.decile, 
         data = Students %>% 
           mutate(ACT.decile = ntiles(ACT, 10, format = "mean")) %>%
           group_by(ACT.decile) %>% 
           summarise(prop.grad = prop(grad))
) %>% 
  gf_fun(fit1(act) ~ act)
sat.glm <- glm(grad ~ SAT, data = Students, family = "binomial")
fit2 <- makeFun(sat.glm)
gf_point(
  prop.grad ~ SAT.decile,
  data = Students %>% 
    mutate(SAT.decile = ntiles(SAT, 10, format = "mean")) %>%
    group_by(SAT.decile) %>% 
    summarise(prop.grad = prop(grad))
) %>%
  gf_fun(fit2(sat) ~ sat)

## ----students05----------------------------------------------------------
tally( ~ is.na(ACT) + is.na(SAT), margins = TRUE, 
       data = Students)

## ----students06----------------------------------------------------------
deviance(act.glm)
1 - pchisq(deviance(act.glm), df = 829)  
deviance(sat.glm)
1 - pchisq(deviance(sat.glm), df = 362)  

## ------------------------------------------------------------------------
act2.glm <- glm(grad ~ ACT, family = "gaussian", data = Students)

## ----lm-sim01, seed = 1234-----------------------------------------------
b0 <- 3; b1 <- 5; sigma <- 2       # set model parameters
x <- rep(1:5, each = 4)            # 4 observations at each of 5 values
e <- rnorm(length(x), sd = sigma)  # error term in the model
y <- b0 + b1 * x + e               # build response according to model
model <- lm(y ~ x)
confint(model)

## ----lm-sim02, tidy = FALSE----------------------------------------------
sim <- 
  function(
    b0 = 3, b1 = 5, sigma = 2, 
    x = rep(1:5, each = 4)         # 4 observations at each of 5 values
    ){
    e <- rnorm(length(x), sd = sigma)
    y <- b0 + b1 * x + e
    model <- lm(y ~ x)  
    ci <- confint(model, 2)
    dimnames(ci)[[2]] <- c("lo", "hi")   # provide nicer names
    ci
  }

## ----lm-sim03, tidy = FALSE----------------------------------------------
sim()       # one simulation
Sims <- do(5000) * sim()   # lot of simulations
Sims <-
  Sims %>% 
  mutate(status = ifelse(lo > 5, "hi", ifelse(hi < 5, "lo", "good"))) 

## ----lm-sim04, tidy = FALSE----------------------------------------------
tally( ~ status, data = Sims, format = "prop")
binom.test( ~ status, data = Sims, p = 0.95)

## ----lm-sim05------------------------------------------------------------
chisq.test(tally( ~ status, data = Sims), p = c(0.95, 0.025, 0.025))

## ----lm-sim06, tidy = FALSE----------------------------------------------
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

## ----lm-sim07, tidy = FALSE----------------------------------------------
tally( ~ status, data = Sims2, format = "prop")
binom.test( ~ status, data = Sims2, p = 0.95)
chisq.test(tally( ~ status, data = Sims2), p = c(0.95, 0.025, 0.025))

## ----lm-sim08, tidy = FALSE----------------------------------------------
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

## ----lm-sim09, tidy = FALSE----------------------------------------------
tally( ~ status, data = Sims3) / 5000
binom.test( ~ status, data = Sims3, p = 0.95)
chisq.test(tally( ~ status, data = Sims3), p = c(0.95, 0.025, 0.025))

## ----glm-guassian-sol, tidy = FALSE--------------------------------------
glm(stretch ~ distance, data = elasticband, 
    family = gaussian()) %>% 
  msummary()
lm(stretch ~ distance, data = elasticband) %>% msummary()
lm(stretch ~ distance, data = elasticband) %>% anova()


## ----RegressionVariations, child="RegressionVariations.Rnw", eval=includeChapter[7]----

## ----reg-setup, include = FALSE, cache = FALSE---------------------------
knitr::opts_chunk$set(cache.path = "cache/Reg-")
require(multcomp)
require(effects)
require(fastR2)

## ----punting01, tidy = FALSE---------------------------------------------
punting.lm <- 
  lm(distance ~ rStrength + rFlexibility, data = Punting) 
msummary(punting.lm)
anova(punting.lm)

## ----punting02, fig.keep = "none", tidy = FALSE--------------------------
gf_point(rStrength ~ rFlexibility, data = Punting)
lm(rFlexibility ~ rStrength, data = Punting) %>% msummary()
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r 
r^2

## ----punting02-fig, echo = FALSE, results = "hide", cache = FALSE--------
gf_point(rStrength ~ rFlexibility, data = Punting)
lm(rFlexibility ~ rStrength, data = Punting) %>% msummary()
# if all we want is the correlation coefficient, we can get it directly
r <- cor(rStrength ~ rFlexibility, data = Punting); r 
r^2

## ----punting03, eval = FALSE---------------------------------------------
## plot(punting.lm, w = 1:2)

## ----punting03-fig, echo = FALSE, results = "hide"-----------------------
plot(punting.lm, w = 1:2)

## ----punting04, tidy = FALSE---------------------------------------------
puntingFit <- makeFun(punting.lm)
puntingFit(rStrength = 175, rFlexibility = 100, 
           interval = "confidence")
puntingFit(rStrength = 175, rFlexibility = 100, interval = "prediction")

## ----punting-robustness-sol, opts.label = "fig3"-------------------------
n <- nrow(Punting)
x1 <- Punting$rStrength; x2 <- Punting$rFlexibility
do(11) * {
  y <- -75 + .5 * x1 + 1.5 * x2 + rnorm(n, 0, 15)
  plot(lm(y ~ x1 + x2), w = 1)
}
plot(punting.lm, w = 1)

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

## ----concrete-QR01-------------------------------------------------------
A <- 
  rbind( 
    w1 / vlength(w1)^2, 
    w2 / vlength(w2)^2)
A %*% y

## ----concrete-QR02, tidy = FALSE-----------------------------------------
x1 <- Concretemod$limestone; x2 <- Concretemod$water
X <- cbind(1, x1, x2)
Q <- cbind(
  1 / sqrt(nrow(Concretemod)), 
  v1 / vlength(v1), 
  w2 / vlength(w2)) 
t(Q) %*% Q %>% round(4)           # should be the identity matrix

## ----concrete-QR03-------------------------------------------------------
R <- t(Q) %*% X; R %>% round(4)   # should be upper triangular
Q %*% R %>% round(4)              # should be X
X

## ----concrete-QR04-------------------------------------------------------
solve(R) %*% t(Q) %*% y 

## ----concrete-QR05-------------------------------------------------------
A %>% round(3) 
solve(R) %*% t(Q) %>% round(3)

## ----concrete-QR06-------------------------------------------------------
diag(R) 
c(vlength(v0), vlength(v1), vlength(v2))

## ----qr, tidy = FALSE----------------------------------------------------
QRdata <- data.frame(x = c(1, 1, 5, 5), y = c(1, 2, 4, 6))
qr.model <- lm(y ~ x, data = QRdata)
Q <- qr.model %>% qr() %>% qr.Q(); Q
R <- qr.model %>% qr() %>% qr.R(); R

## ----backsolve-----------------------------------------------------------
backsolve(R, t(Q) %*% QRdata$y)
coef(qr.model)

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

## ----concrete-plot, fig.keep = "none"------------------------------------
plot(concrete.lm0, which = c(1, 2, 3, 5))

## ----concrete-plot-fig, echo = FALSE, results = "hide"-------------------
plot(concrete.lm0, which = c(1, 2, 3, 5))

## ----punting05, fig.keep = "none"----------------------------------------
# original regression with two predictors
punting.lm <- lm( distance ~ rStrength + rFlexibility, data = Punting)
# partial regressions of y and x1 on x2
punting.lmy2 <- lm(distance ~ rFlexibility, data = Punting)
punting.lm12 <- lm(rStrength ~ rFlexibility, data = Punting)
# residuals vs residuals
punting.rvr1 <- lm(resid(punting.lmy2) ~ resid(punting.lm12))
# the slope of rvr matches the coefficient from y ~ x1 + x2; intercept is 0
coef(punting.rvr1) %>% round(4)
coef(punting.lm) %>% round(4)
# rvr and original model have the same residuals
gf_point(resid(punting.rvr1) ~ resid(punting.lm))

## ----punting05-fig, echo = FALSE, results = "hide"-----------------------
# original regression with two predictors
punting.lm <- lm( distance ~ rStrength + rFlexibility, data = Punting)
# partial regressions of y and x1 on x2
punting.lmy2 <- lm(distance ~ rFlexibility, data = Punting)
punting.lm12 <- lm(rStrength ~ rFlexibility, data = Punting)
# residuals vs residuals
punting.rvr1 <- lm(resid(punting.lmy2) ~ resid(punting.lm12))
# the slope of rvr matches the coefficient from y ~ x1 + x2; intercept is 0
coef(punting.rvr1) %>% round(4)
coef(punting.lm) %>% round(4)
# rvr and original model have the same residuals
gf_point(resid(punting.rvr1) ~ resid(punting.lm))

## ----punting06, fig.keep = "none"----------------------------------------
# partial regressions of y and x2 on x1
punting.lmy1 <- lm(distance ~ rStrength, data = Punting)
punting.lm21 <- lm(rFlexibility ~ rStrength, data = Punting)
# residuals vs residual
punting.rvr2 <- lm(resid(punting.lmy1) ~ resid(punting.lm21))
# partial regression plots (a.k.a. added-variable plots)
gf_lm(resid(punting.lmy2) ~ resid(punting.lm12)) %>%
  gf_point()
gf_lm(resid(punting.lmy1) ~ resid(punting.lm21)) %>%
  gf_point()

## ----punting07, opts.label = "fig1", fig.keep = "none"-------------------
car::avPlots(punting.lm, id.n = 1)

## ----punting06-fig, echo = FALSE, results = "hide"-----------------------
# partial regressions of y and x2 on x1
punting.lmy1 <- lm(distance ~ rStrength, data = Punting)
punting.lm21 <- lm(rFlexibility ~ rStrength, data = Punting)
# residuals vs residual
punting.rvr2 <- lm(resid(punting.lmy1) ~ resid(punting.lm21))
# partial regression plots (a.k.a. added-variable plots)
gf_lm(resid(punting.lmy2) ~ resid(punting.lm12)) %>%
  gf_point()
gf_lm(resid(punting.lmy1) ~ resid(punting.lm21)) %>%
  gf_point()

## ----punting07-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
car::avPlots(punting.lm, id.n = 1)

## ----punting08, fig.keep = "none"----------------------------------------
punting.lm3 <-
  lm(distance ~ rStrength + rFlexibility, Punting[-3, ])
punting.lm %>% summary() %>% coef()
punting.lm3 %>% summary() %>% coef()
car::avPlots(punting.lm3, id.n = 1)

## ----punting08-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
punting.lm3 <-
  lm(distance ~ rStrength + rFlexibility, Punting[-3, ])
punting.lm %>% summary() %>% coef()
punting.lm3 %>% summary() %>% coef()
car::avPlots(punting.lm3, id.n = 1)

## ----concrete-effects01, fig.keep = "none"-------------------------------
require(effects)
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("water")

## ----concrete-effects02, fig.keep = "none"-------------------------------
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("limestone")

## ----concrete-effects01-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
require(effects)
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("water")

## ----concrete-effects02-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
Effect(c("water", "limestone"), concrete.lm0) %>% 
  plot("limestone")

## ----concrete-effects03, fig.keep = "none"-------------------------------
Effect(
  c("water", "limestone"), concrete.lm0,  partial.resid = TRUE) %>% 
  plot("water")

## ----concrete-effects03-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
Effect(
  c("water", "limestone"), concrete.lm0,  partial.resid = TRUE) %>% 
  plot("water")

## ----concrete-effect04---------------------------------------------------
concrete.lm6 <- 
  lm(strength ~ limestone + water + limestone:water, data = Concrete)

## ----concrete-effect05, fig.keep = "none", opts.label = "fig1"-----------
lm(strength ~ limestone + water + limestone:water, 
   data = Concrete) %>%
  Effect(c("water", "limestone"), . , partial.residuals = TRUE) %>%
  plot("water")

## ----concrete-effect05-fig, echo = FALSE, results = "hide", opts.label = "fig1"----
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
  plot("x1", main = "additive model; y1")
lm(y1 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y1")
lm(y2 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "additive model; y2")
lm(y2 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y2")

## ----effect-sim-fig, echo = FALSE, results = "hide", seed = 1234, opts.label = "fig1"----
D <- data_frame(
  x1 = runif(100, 0, 10),
  x2 = runif(100, 0, 10),
  y1 = 5 + 2 * x1 + 3 * x2 + rnorm(100, sd = 4),
  y2 = 5 + 2 * x1 + 3 * x2 - x1 * x2 + rnorm(100, sd = 4)
)
lm(y1 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "additive model; y1")
lm(y1 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y1")
lm(y2 ~ x1 + x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "additive model; y2")
lm(y2 ~ x1 + x2 + x1*x2, data = D) %>%
  Effect(c("x1", "x2"), ., partial.residuals = TRUE) %>%
  plot("x1", main = "interaction model; y2")

## ----utilities-kwh01-----------------------------------------------------
Utilities2 <- 
  Utilities %>% 
  filter(year > 2000 | month > 6) %>%  # remove bad meter reading
  filter(temp <= 60) %>%               # remove warm months 
  mutate(kwhpday = kwh / billingDays)

## ----utilities-kwh02, fig.keep = "none"----------------------------------
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
  plot("temp", sub = "interation model", alternating = FALSE)

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
  plot("temp", sub = "interation model", alternating = FALSE)

## ----utilities-kwh03-----------------------------------------------------
coef(ut.lmint)[1] +  coef(ut.lmint)[3] * 25
coef(ut.lmint)[2] +  coef(ut.lmint)[4] * 25
coef(ut.lm)

## ----utilities-month01, fig.keep = "none"--------------------------------
# remove first few observations because of bad meter read
Utilities3 <- 
  Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Utilities3)
msummary(ut.lm3)
fit3 <- makeFun(ut.lm3)
gf_point(thermsPerDay ~ month, data = Utilities3) %>%
  gf_function(fit3, color = "red", alpha = 0.6)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month") 
plot(ut.lm3, w = 1:2)

## ----utilities-month-fig, echo = FALSE, results = "hide"-----------------
# remove first few observations because of bad meter read
Utilities3 <- 
  Utilities %>% filter(year > 2000 | month > 6)
ut.lm3 <- lm(thermsPerDay ~ month + I(month^2), data = Utilities3)
msummary(ut.lm3)
fit3 <- makeFun(ut.lm3)
gf_point(thermsPerDay ~ month, data = Utilities3) %>%
  gf_function(fit3, color = "red", alpha = 0.6)
ut.lm3 %>% Effect("month", ., partial.residuals = TRUE) %>% plot("month") 
plot(ut.lm3, w = 1:2)

## ----utilities-month02, fig.keep = "none"--------------------------------
Utilities3 <- Utilities3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- 
  lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Utilities3)
msummary(ut.lm4)
fit4 <- makeFun(ut.lm4)
gf_point(thermsPerDay ~ monthShifted, data = Utilities3) %>%
  gf_function(fit4, color = "red", alpha = 0.6)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

## ----utilities-month02-fig, echo = FALSE, results = "hide"---------------
Utilities3 <- Utilities3 %>% 
  mutate(monthShifted  = (month - 2) %% 12)
ut.lm4 <- 
  lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Utilities3)
msummary(ut.lm4)
fit4 <- makeFun(ut.lm4)
gf_point(thermsPerDay ~ monthShifted, data = Utilities3) %>%
  gf_function(fit4, color = "red", alpha = 0.6)
ut.lm4 %>% Effect("monthShifted", ., partial.residuals = TRUE) %>% 
  plot("monthShifted")
plot(ut.lm4, w = 1:2)

## ----utilities-month03---------------------------------------------------
ut.lm4a <- 
  lm(thermsPerDay ~ poly(monthShifted, 2), data = Utilities3)
msummary(ut.lm4a)
favstats( ~ (fitted(ut.lm4a) - fitted(ut.lm4)))

## ----utilities-month-sol-------------------------------------------------
ut.mod <-
  Utilities3 %>% 
  mutate(monthShifted2 = (month + 5) %% 12) %>%
  lm(thermsPerDay ~ poly(monthShifted2, 2), data = .) 
msummary(ut.mod)
plot(ut.mod, 1:2)
ut.mod %>% Effect("monthShifted2", ., partial.residuals = TRUE) %>%
  plot("monthShifted2")

## ----eval = FALSE--------------------------------------------------------
## px <- poly(Utilities2$monthShifted, 2); px

## ------------------------------------------------------------------------
Utilities3 <- 
  Utilities %>% filter(year > 2000 | month > 6) %>%
  mutate(monthShifted  = (month - 2) %% 12)
model1 <- 
  lm(thermsPerDay ~ monthShifted, data = Utilities3) 
model2 <- 
  lm(thermsPerDay ~ monthShifted + I(monthShifted^2), data = Utilities3)
model1poly <- 
  lm(thermsPerDay ~ poly(monthShifted, 1),  data = Utilities3)
model2poly <- 
  lm(thermsPerDay ~ poly(monthShifted, 2),  data = Utilities3)

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

## ----pheno-weight03, fig.keep = "none"-----------------------------------
plot(pheno.lm, w = 1:2)

## ----pheno-weight-fig, echo = FALSE, results = "hide"--------------------
plot(pheno.lm, w = 1:2)

## ----pheno-weight04, fig.keep = "none"-----------------------------------
gf_dhistogram( ~ fitted(pheno.lm))
gf_boxplot(resid(pheno.lm) ~ ntiles(fitted(pheno.lm), 10)) %>%
  gf_labs(x = "fitted value deciles")

## ----pheno-weight04-fig, echo = FALSE, results = "hide"------------------
gf_dhistogram( ~ fitted(pheno.lm))
gf_boxplot(resid(pheno.lm) ~ ntiles(fitted(pheno.lm), 10)) %>%
  gf_labs(x = "fitted value deciles")

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

## ----coag01, fig.keep = "none"-------------------------------------------
data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
gf_point(coag ~ diet, data = coagulation)
gf_boxplot(coag ~ diet, data = coagulation)

## ----coag01-fig, echo = FALSE, results = "hide"--------------------------
data(coagulation, package = "faraway") 
favstats(coag ~ diet, data = coagulation)
gf_point(coag ~ diet, data = coagulation)
gf_boxplot(coag ~ diet, data = coagulation)

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

# individual tally
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
mplot(TukeyHSD(airp.lm), system = "gg") %>% 
  gf_theme(legend.position = "top") %>%
  gf_labs(title = "")
  

## ----airp-glht01-fig, echo = FALSE, results = "hide", cache = FALSE------
require(multcomp)   
airp.cint <- confint(glht(airp.lm, mcp(location = "Tukey")))
airp.cint  
plot(airp.cint)
mplot(TukeyHSD(airp.lm), system = "gg") %>% 
  gf_theme(legend.position = "top") %>%
  gf_labs(title = "")
  
# plot(airp.cint)
# mplot(TukeyHSD(airp.lm), system = "gg") 

## ----coag-TukeyHSD-------------------------------------------------------
coag.lm <- lm(coag ~ diet, data = coagulation)
TukeyHSD(coag.lm)

## ----coag-glht, fig.keep = "none"----------------------------------------
require(multcomp)
coag.glht <- glht(coag.lm, mcp(diet = "Tukey"))
msummary(coag.glht)  
plot(confint(coag.glht))
mplot(TukeyHSD(coag.lm), system = "gg") %>%
  gf_theme(legend.position = "top") 

## ----coag-glht-fig, echo = FALSE, results = "hide", message = FALSE------
require(multcomp)
coag.glht <- glht(coag.lm, mcp(diet = "Tukey"))
msummary(coag.glht)  
plot(confint(coag.glht))
mplot(TukeyHSD(coag.lm), system = "gg") %>%
  gf_theme(legend.position = "top") 

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
contr3 <- 
  mcp(location = rbind(
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

## ----cholesterol01, fig.keep = "none"------------------------------------
data(cholesterol, package = "multcomp")
cholesterol <- cholesterol %>% 
  mutate(trt = factor(gsub("drug", "", gsub("times*", "x", trt))))
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

## ----cholesterol01-fig, echo = FALSE, results = "hide"-------------------
data(cholesterol, package = "multcomp")
cholesterol <- cholesterol %>% 
  mutate(trt = factor(gsub("drug", "", gsub("times*", "x", trt))))
chol.lm <- lm(response ~ trt, data = cholesterol)
plot(chol.lm, w = c(5, 2))       # diagnostic plots
msummary(chol.lm)
anova(chol.lm)

## ----cholesterol02, fig.keep = "none"------------------------------------
chol.glht <- confint(glht(chol.lm, mcp(trt = "Tukey")))
msummary(chol.glht)
plot(confint(chol.glht))

## ----cholesterol02-fig, echo = FALSE, results = "hide", opts.label = "figtall"----
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

## ----dunnet--------------------------------------------------------------
glht(coag.lm, mcp(diet = "Dunnet")) %>% 
  summary()

## ----taste-anova01, fig.keep = "none"------------------------------------
favstats(score ~ type, data = TasteTest)
gf_point(score ~ type, data = TasteTest)
taste.lm <- lm(score ~ type, data = TasteTest)
anova(taste.lm)
taste.cint <- confint(glht(taste.lm, mcp(type = "Tukey"))); taste.cint
plot(taste.cint)

## ----taste-anova01-fig, echo = FALSE, results = "hide"-------------------
favstats(score ~ type, data = TasteTest)
gf_point(score ~ type, data = TasteTest)
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

## ----taste-anova02-------------------------------------------------------
df_stats(score ~ scr + liq, data = TasteTest, mean, sd)

## ----taste-anova03-------------------------------------------------------
taste.lm <- lm(score ~ scr * liq, data = TasteTest)
anova(taste.lm)

## ----taste-anova04-------------------------------------------------------
taste.lm <- lm(score ~ scr * liq, data = TasteTest)
msummary(taste.lm)

## ----taste-anova05-------------------------------------------------------
M <- cbind(                                # model matrix
        "C1" = rep(c(-1, -1, 1, 1), each = 4)/8,     # C1
        "C2" = rep(c(-1, 1, -1, 1), each = 4)/8,     # C2
        "C3" = rep(c(1, -1, -1, 1), each = 4)/4      # C3
        )
taste.lm2 <- lm(score ~ M, data = TasteTest)
msummary(taste.lm2)

## ----taste-anova06-------------------------------------------------------
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

## ----noise03, fig.keep = "none"------------------------------------------
gf_jitter(score ~ noise, color = ~ group, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ group, fun.data = mean_se)

gf_jitter(score ~ group, color = ~ noise, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ noise, fun.data = mean_se)

## ----noise03-fig, echo = FALSE, results = "hide", seed = 1234------------
gf_jitter(score ~ noise, color = ~ group, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ group, fun.data = mean_se)

gf_jitter(score ~ group, color = ~ noise, data = MathNoise,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ noise, fun.data = mean_se)

## ----poison01------------------------------------------------------------
poison.lm <- 
  lm(time ~ factor(poison) * factor(treatment), data = Poison)
anova(poison.lm)

## ----poison02, fig.keep = "none"-----------------------------------------
plot(poison.lm, w = 1:2)

## ----poison02-fig, echo = FALSE, results = "hide"------------------------
plot(poison.lm, w = 1:2)

## ----poison03, fig.keep = "none"-----------------------------------------
poison.lm2 <- 
  lm(1/time ~ factor(poison) * factor(treatment), data = Poison)
plot(poison.lm2, w = 1:2)

## ----poison03-fig, echo = FALSE, results = "hide"------------------------
poison.lm2 <- 
  lm(1/time ~ factor(poison) * factor(treatment), data = Poison)
plot(poison.lm2, w = 1:2)

## ----poison-trans-anova--------------------------------------------------
anova(poison.lm2)

## ----pallets01-----------------------------------------------------------
pallets.lm1 <- lm(pallets ~ employee, data = Pallets)
anova(pallets.lm1)

## ----pallets02-----------------------------------------------------------
pallets.lm2 <- lm(pallets ~ day + employee, data = Pallets)
anova(pallets.lm2)

## ----pallets03, fig.keep = "none"----------------------------------------
gf_point(pallets ~ day, data = Pallets, 
         color = ~ employee) %>%
  gf_line(group = ~ employee)

## ----pallets03-fig, tidy = FALSE, echo = FALSE, results = "hide"---------
gf_point(pallets ~ day, data = Pallets, 
         color = ~ employee) %>%
  gf_line(group = ~ employee)

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

## ----seatpos01-----------------------------------------------------------
data(seatpos, package = "faraway")
seatpos.lm1 <- lm(hipcenter ~ ., data = seatpos)
msummary(seatpos.lm1)

## ----seatpos02-----------------------------------------------------------
faraway::vif(seatpos.lm1)

## ----seatpos03, fig.keep = "none"----------------------------------------
round(cor(seatpos), 2)
GGally::ggpairs(seatpos)
corrgram::corrgram(seatpos, order = TRUE)

## ----seatpos03-fig, echo = FALSE, results = "hide", opts.label = "figbig"----
round(cor(seatpos), 2)
GGally::ggpairs(seatpos)
corrgram::corrgram(seatpos, order = TRUE)

## ----seatpos04-----------------------------------------------------------
seatpos.lm2 <- lm(hipcenter ~ Age + Weight + Ht, data = seatpos)
msummary(seatpos.lm2)
faraway::vif(seatpos.lm2)

## ----seatpos05-----------------------------------------------------------
pc <- with(seatpos, 
          princomp(cbind(HtShoes, Ht, Seated, Arm, Thigh, Leg),
                   scores = TRUE))
msummary(pc, loadings = TRUE)
seatpos.lmpc <-lm(hipcenter ~ Age + Weight + pc$scores[, 1], data = seatpos)
msummary(seatpos.lmpc)
faraway::vif(seatpos.lmpc)

## ----eval = FALSE--------------------------------------------------------
## x <- 0.65*HtShoes + 0.65*Ht + 0.27*Seated + 0.15*Arm + 0.17*Thigh + 0.18*Leg

## ----seatpos06-----------------------------------------------------------
# trace=0 turns off intermediate reporting
seatpos.lmstep <- step(seatpos.lm1, trace = 0)  
msummary(seatpos.lmstep)
faraway::vif(seatpos.lmstep)

## ----students-gpa--------------------------------------------------------
act.glm <- 
  glm(grad ~ ACT, data = Students, family = "binomial")
gpa.glm <- 
  glm(grad ~ hsGPA, data = Students, family = "binomial")
actgpa.glm <- 
  glm(grad ~ ACT + hsGPA, data = Students, family = "binomial")
msummary(actgpa.glm) %>% coef()
c(gpa    = deviance(gpa.glm),
  act    = deviance(act.glm),
  actgpa = deviance(actgpa.glm))
# small p-value suggests that adding gpa is helpful
1 - pchisq(deviance(act.glm) - deviance(actgpa.glm), df = 2)
# larger p-value here compared with act.glm suggests better fit
1 - pchisq(deviance(actgpa.glm), df = df.residual(actgpa.glm))

## ----ice01, fig.keep = "none", message = FALSE---------------------------
require(tidyverse)
Ice2 <- 
  Ice %>% 
  gather("key", "temp", b0:r12000) %>% 
  separate(key, c("phase", "time"), sep = 1) %>% 
  mutate(time = readr::parse_number(time), subject = as.character(subject))  
Ice2 %>% filter(phase == "t") %>% 
  gf_line(temp ~ time, group = ~ subject, color = ~sex) %>%
  gf_facet_grid( treatment ~ location, scales = "free_x") %>%
  gf_labs(
    title = "Temperature during treatment phase (3 conditions, 2 locations)")

## ----ice01-fig, opts.label = "fig1", echo = FALSE, results = "hide"------
require(tidyverse)
Ice2 <- 
  Ice %>% 
  gather("key", "temp", b0:r12000) %>% 
  separate(key, c("phase", "time"), sep = 1) %>% 
  mutate(time = readr::parse_number(time), subject = as.character(subject))  
Ice2 %>% filter(phase == "t") %>% 
  gf_line(temp ~ time, group = ~ subject, color = ~sex) %>%
  gf_facet_grid( treatment ~ location, scales = "free_x") %>%
  gf_labs(
    title = "Temperature during treatment phase (3 conditions, 2 locations)")

## ----ice02---------------------------------------------------------------
Ice2 %>% filter(time == 1930, phase == "b") %>%
  group_by(location, treatment, phase) %>%
  summarise(mean(temp))

## ----ice03, digits = 4---------------------------------------------------
Ice1930 <- Ice2 %>% filter(time == 1930)
base.lmint <- 
  lm(temp ~ location * treatment, data = Ice1930 %>% filter(phase == "b"))
anova(base.lmint)

## ----ice04, fig.keep = "none"--------------------------------------------
base.lmadd <- 
  lm(temp ~ location + treatment, data = Ice1930 %>% filter(phase == "b"))
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

## ----ice04-fig, echo = FALSE, results = "hide"---------------------------
base.lmadd <- 
  lm(temp ~ location + treatment, data = Ice1930 %>% filter(phase == "b"))
anova(base.lmadd)
plot(base.lmadd, w = c(5, 2))

## ----ice05---------------------------------------------------------------
require(multcomp)  
confint(glht(base.lmadd, mcp(treatment = "Tukey")), level = 0.9)

## ----ice06---------------------------------------------------------------
ice.trt <- lm(t1930 - b1930 ~ treatment * location, data = Ice)
anova(ice.trt)

## ----ice07---------------------------------------------------------------
ice.trt2 <- lm(t1930 - b1930 ~ treatment, data = Ice,
                 subset = location == "intramuscular")
msummary(ice.trt2)
confint(glht(ice.trt2, mcp(treatment = "Tukey")), level = 0.90)

## ----ice08, digits = 4---------------------------------------------------
Ice3 <- 
  Ice2 %>% filter(time == 1930) %>%
  spread(location, temp)
anova(lm(surface - intramuscular ~ treatment, 
         data = Ice3 %>% filter(phase == "t")))

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

## ----step, fig.keep = "none"---------------------------------------------
step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
gf_line(HR - restHR ~ freq, data = Step, color = ~height, 
        group = ~ height, stat = "summary", fun.data = mean_se) %>%
  gf_jitter(width = 0.15, height = 0)

## ----step-fig, echo = FALSE, results = "hide"----------------------------
step.lm <- lm(HR - restHR ~ height * freq, data = Step)
msummary(step.lm)
anova(step.lm)
gf_line(HR - restHR ~ freq, data = Step, color = ~height, 
        group = ~ height, stat = "summary", fun.data = mean_se) %>%
  gf_jitter(width = 0.15, height = 0)

## ----rat01, fig.keep = "none"--------------------------------------------
rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
gf_point(consumption ~ flavor, color = ~ location, data = RatPoison,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ location, fun.data = mean_se) 

## ----rat01-fig, echo = FALSE, results = "hide"---------------------------
rat.lm <- lm(consumption ~ location + flavor, data = RatPoison)
anova(rat.lm)
plot(rat.lm, w=c(1, 2, 5))
gf_point(consumption ~ flavor, color = ~ location, data = RatPoison,
          width = 0.15, height = 0) %>%
  gf_line(stat = "summary", group = ~ location, fun.data = mean_se) 

## ----rat02---------------------------------------------------------------
rat.lm1 <- lm(consumption ~ flavor, data = RatPoison)
anova(rat.lm)
anova(rat.lm1)
summary(rat.lm)$sigma
summary(rat.lm1)$sigma
summary(rat.lm1)$sigma^2/summary(rat.lm)$sigma^2

## ----concrete-perm01-----------------------------------------------------
concrete.lm <- 
  lm(strength ~ limestone + water, data = Concrete)

## ----concrete-perm02-----------------------------------------------------
msummary(concrete.lm)
obsF <- msummary(concrete.lm)$fstat[1]; obsF

## ----concrete-perm03, seed = 12345, digits = 4---------------------------
Null.F <- 
  do(5000) * lm(shuffle(strength) ~ limestone + water, data = Concrete)
Null.F %>% head(3)
prop1( ~ (F >= obsF), data = Null.F) 

## ----concrete-perm04, seed = 12345---------------------------------------
Null.t2 <-   
  do(10000) * {
    lm(strength ~ shuffle(limestone) + water, data = Concrete) %>%
    coef() 
  }
  
Null.t3 <- 
  do(10000) * {
      lm(strength ~ limestone + shuffle(water), data = Concrete) %>%
      coef()
  }
Null.t2 %>% head(3)
2 * prop1( ~ (limestone >= coef(concrete.lm)[2]), data = Null.t2)
2 * prop1( ~ (water <= coef(concrete.lm)[3]), data = Null.t3)

## ----concrete-perm-sol---------------------------------------------------
gf_dhistogram( ~ Intercept, data = Null.F, bins = 25) %>%
  gf_vline(xintercept = c(0, mean(~strength, data = Concrete)))

## ----smoke-perm01--------------------------------------------------------
smokeTab <- 
  tally( ~ student + parents, data = FamilySmoking) 
smokeTab
chisq.test(smokeTab)
observedStat <- chisq.test(smokeTab) %>% stat(); observedStat
Stats <- do(2000) * {
  tally( ~ shuffle(student) + parents,  data = FamilySmoking) %>%
  chisq.test() %>%
  stat()
}
prop( ~ (X.squared >= observedStat), data = Stats) 
binom.test(0, 2000, alternative = "less") %>% confint()

## ----smoke-perm03, seed = 1234, digits = 4-------------------------------
chisq.test(smokeTab, simulate.p.value = TRUE)

## ----smoke-perm02, seed = 12345, digits = 4------------------------------
chisq.test(smokeTab, simulate.p.value = TRUE, B = 10000) %>% 
  pval()
chisq.test(smokeTab, simulate.p.value = TRUE, B = 100000) %>% pval()

## ----balldrop-nls01, tidy=FALSE------------------------------------------
balldrop.nls <- 
  nls(time ~ alpha0 + alpha1 * height^d, 
      data = BallDrop, 
      start = list(alpha0 = 0, alpha1 = 1, d = 1))

## ----balldrop-nls02------------------------------------------------------
balldrop.nls %>% coef()

## ----balldrop-nls03------------------------------------------------------
balldrop.nls %>% summary() 

## ----balldrop-nls04------------------------------------------------------
balldrop.nls %>% summary() %>% coef()
balldrop.nls %>% confint()

## ----balldrop-nls05, fig.keep = "none"-----------------------------------
f <- makeFun(balldrop.nls)
gf_point( time ~ height, data = BallDrop ) %>%
  gf_fun(f(height) ~ height, alpha = 0.4)
plot(balldrop.nls)
gf_point(resid(balldrop.nls) ~ fitted(balldrop.nls))  # unstandardized resids

## ----balldrop-nls05-fig, fig.keep = 2:4, results = "hide", echo = FALSE----
f <- makeFun(balldrop.nls)
gf_point( time ~ height, data = BallDrop ) %>%
  gf_fun(f(height) ~ height, alpha = 0.4)
plot(balldrop.nls)
gf_point(resid(balldrop.nls) ~ fitted(balldrop.nls))  # unstandardized resids

## ----balldrop-nls06, fig.keep='none'-------------------------------------
balldrop.lm <- lm( log(time) ~ log(height), data = BallDrop)
balldrop.lm  %>% coef()
balldrop.lm  %>% coef() %>% getElement(1) %>% exp()
balldrop.nls %>% coef()
g <- makeFun(balldrop.lm)
gf_point(time ~ height, data = BallDrop) %>%
  gf_fun(f(height) ~ height, alpha = 0.4, size = 0.8) %>% 
  gf_fun(g(height) ~ height, col = "red", 
         linetype = 2, alpha = 0.7, size = 0.8)

## ----balldrop-nls06-fig, fig.keep='last', results = "hide", echo = FALSE, opts.label = "fig1"----
balldrop.lm <- lm( log(time) ~ log(height), data = BallDrop)
balldrop.lm  %>% coef()
balldrop.lm  %>% coef() %>% getElement(1) %>% exp()
balldrop.nls %>% coef()
g <- makeFun(balldrop.lm)
gf_point(time ~ height, data = BallDrop) %>%
  gf_fun(f(height) ~ height, alpha = 0.4, size = 0.8) %>% 
  gf_fun(g(height) ~ height, col = "red", 
         linetype = 2, alpha = 0.7, size = 0.8)

## ----balldrop-nls07, fig.keep = "none"-----------------------------------
plot(balldrop.nls)
plot(balldrop.lm, w = 1)

## ----balldrop-nls08, fig.keep = "none"-----------------------------------
gf_qq( ~ resid(balldrop.nls))
gf_qq( ~ resid(balldrop.lm))

## ----balldrop-nls09, fig.keep = "none"-----------------------------------
gf_point(resid(balldrop.nls) ~ f(BallDrop$height))
gf_point(resid(balldrop.lm)  ~ g(BallDrop$height))

## ----balldrop-nls07-fig, results = "hide", echo = FALSE------------------
plot(balldrop.nls)
plot(balldrop.lm, w = 1)
gf_qq( ~ resid(balldrop.nls))
gf_qq( ~ resid(balldrop.lm))
gf_point(resid(balldrop.nls) ~ f(BallDrop$height))
gf_point(resid(balldrop.lm)  ~ g(BallDrop$height))

## ----balldrop-nls10------------------------------------------------------
balldrop.lm %>% confint()
balldrop.nls %>% confint()

## ----cooling01, fig.keep = "none"----------------------------------------
gf_line(temp ~ time, data = CoolingWater1) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

## ----cooling01-fig, echo = FALSE-----------------------------------------
gf_line(temp ~ time, data = CoolingWater1) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

## ----cooling02, tidy=FALSE, fig.keep='none'------------------------------
cooling.model1 <- 
  nls(temp ~ A * exp( -k * time), data = CoolingWater1, 
      start = list(A = 100, k = 0.01))
f1 <- makeFun(cooling.model1)
gf_point(temp ~ time, data = CoolingWater1, size = 0.6) %>%
  gf_fun(f1(time) ~ time, lty = 2, col = "gray60") %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

## ----cooling03, tidy = FALSE, fig.keep = "none"--------------------------
cooling.model2 <- 
  nls(temp ~ ambient + A * exp( - k * (1 + time)), 
      data = CoolingWater1,
      start = list(ambient = 20, A = 80, k = 0.01) )
f2 <- makeFun(cooling.model2)
gf_point(temp ~ time, data = CoolingWater1) %>%  
  gf_fun(f1(time) ~ time, lty = 2, col = "gray60") %>%
  gf_fun(f2(time) ~ time, col = "red", size = 0.8) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

## ----cooling03-fig, echo = FALSE, opts.label = "fig1"--------------------
cooling.model2 <- 
  nls(temp ~ ambient + A * exp( - k * (1 + time)), 
      data = CoolingWater1,
      start = list(ambient = 20, A = 80, k = 0.01) )
f2 <- makeFun(cooling.model2)
gf_point(temp ~ time, data = CoolingWater1) %>%  
  gf_fun(f1(time) ~ time, lty = 2, col = "gray60") %>%
  gf_fun(f2(time) ~ time, col = "red", size = 0.8) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")

## ----cooling04, fig.keep = "none"----------------------------------------
gf_point(resid(cooling.model2) ~ time, data = CoolingWater1) 
plot(cooling.model2)

## ----cooling04-fig, echo = FALSE-----------------------------------------
gf_point(resid(cooling.model2) ~ time, data = CoolingWater1) 
plot(cooling.model2)

## ----cooling-water-sol---------------------------------------------------
cooling.model3 <- 
  nls(temp ~ ambient + A * exp( - k * (1 + time)), 
      data = CoolingWater2,
      start = list(ambient = 20, A = 80, k = 0.01) )
f3 <- makeFun(cooling.model3)
gf_point(temp ~ time, data = CoolingWater2) %>%  
  gf_fun(f3(time) ~ time, col = "red", size = 0.8) %>%
  gf_labs(y = "temp (C)", x = "time (sec)")
gf_point(resid(cooling.model3) ~ time, data = CoolingWater2)
gf_point(resid(cooling.model3) ~ fitted(cooling.model3))

## ----rat-liver-----------------------------------------------------------
data(rat, package = "alr3")  
rat.lm <- lm(y ~ BodyWt * LiverWt, data = rat)
msummary(rat.lm)

## ----eggprod01-sol-------------------------------------------------------
data(eggprod, package = "faraway")
eggprod.lm <- lm(eggs ~ block + treat, data = eggprod)
anova(eggprod.lm)

## ----eggprod02-sol-------------------------------------------------------
msummary(eggprod.lm)
coef(msummary(eggprod.lm))

## ----eggprod03-sol-------------------------------------------------------
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
C_hat <- 
  sum( c(1, 2, -3) * mean(pollution ~ location, data = AirPollution)) 
C_hat
t <- (C_hat - 0) / (s / kappa)
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

## ----include = FALSE, cache = FALSE--------------------------------------
knitr::opts_chunk$set(cache.path = "cache/R-") 
require(faraway)
require(car)
require(tidyr)
require(ggformula)
theme_set(theme_bw())

## ----install-tidyr, eval = FALSE-----------------------------------------
## # fetch package from CRAN to local machine.
## install.packages("tidyr")
## # load (and attach) the package so it can be used.
## require(tidyr)      # or library(tidyr)

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
## # adjust ggplot2/ggformula settings
## theme_set(theme_bw())

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
args(sum)

## ----usage---------------------------------------------------------------
formatR::usage(sum)

## ----example, eval = FALSE-----------------------------------------------
## example(gf_histogram)

## ----savehistory, eval = FALSE-------------------------------------------
## savehistory("someRCommandsIalmostLost.R")

## ----package-data--------------------------------------------------------
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

## ----c01-----------------------------------------------------------------
myData1 <- c(15, 18, 12, 21, 23, 50, 15)
myData2 <- c("red", "red", "orange", "green", "blue", "blue", "red")

## ----c02-----------------------------------------------------------------
is.vector(myData1)
is.vector(myData2)

## ----c03-----------------------------------------------------------------
x <- c(first = 10, second = 20); x
names(x)                        # what are the names?
x["first"]
x[1]
y <- 1:3                        # vector without names
names(y) <- c("A", "B", "C")    # names added
y
as.vector(y)                    # vector without the names

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
gf_histogram( ~ rnorm(1000, mean = 10, sd = 2), binwidth = 1)

## ----generating-data02-fig, echo = FALSE, results = "hide"---------------
rnorm(10, mean = 10, sd = 2)  # random normal draws
gf_histogram( ~ rnorm(1000, mean = 10, sd = 2), binwidth = 1)

## ----generating-data03, seed = 12345-------------------------------------
sample(Births78, 3)       # sample 3 rows from Births78
Births78 %>% sample(3)    # sample 3 rows from Births78
sample(1:10, size = 5)    # random sample of size 5 (w/o replacement)
resample(1:10, size = 10) # random sample of size 10 (w/ replacement)

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
save(SomeData, greeting, file = "mystuff.rda")  # saves both in 1 file
load("mystuff.rda")                             # loads both

## ------------------------------------------------------------------------
saveRDS(SomeData, file = "SomeData.rds")
EEE <- readRDS("SomeData.rds")
EEE

## ----mutate01, fig.keep = "none"-----------------------------------------
data(Births78)
Births78 <- 
  mutate(Births78, runningTotal = cumsum(births))
head(Births78, 3)
gf_line(runningTotal ~ date, data = Births78)

## ----mutate-fig, echo = FALSE, results = "hide"--------------------------
data(Births78)
Births78 <- 
  mutate(Births78, runningTotal = cumsum(births))
head(Births78, 3)
gf_line(runningTotal ~ date, data = Births78)

## ----mutate02------------------------------------------------------------
CPS85 <- mutate(CPS85, workforce.years = age - 6 - educ)
favstats( ~ workforce.years, data = CPS85)

## ----mutate03------------------------------------------------------------
tally( ~ (exper - workforce.years), data = CPS85)

## ----mutate04, tidy = FALSE----------------------------------------------
HELP2 <- mutate( HELPrct, 
  newsex = factor(female, labels = c("M", "F")) )

## ----mutate05------------------------------------------------------------
tally( ~ newsex + female, data = HELP2 )

## ----derivedFactor, tidy = FALSE, fig.keep = "none"----------------------
HELP3 <-
  mutate(
    HELPrct, 
    risklevel = derivedFactor(
      low = sexrisk < 5, 
      medium = sexrisk < 10,
      high = sexrisk >= 10,
      .method = "first"      # use first rule that applies
    )
  )
gf_jitter(sexrisk ~ risklevel, data = HELP3, 
          height = 0.2, width = 0.3, alpha = 0.4)

## ----derivedFactor-fig, echo = FALSE, results = "hide"-------------------
HELP3 <-
  mutate(
    HELPrct, 
    risklevel = derivedFactor(
      low = sexrisk < 5, 
      medium = sexrisk < 10,
      high = sexrisk >= 10,
      .method = "first"      # use first rule that applies
    )
  )
gf_jitter(sexrisk ~ risklevel, data = HELP3, 
          height = 0.2, width = 0.3, alpha = 0.4)

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
## h(g(f(x), y), z)
## f(x) %>% g(y) %>% h(z)

## ----eval = FALSE--------------------------------------------------------
## bop(scoop(hop(foo_foo, through = forest), up = field_mice), on = head)

## ----eval = FALSE--------------------------------------------------------
## foo_foo %>%
##   hop(through = forest) %>%
##   scoop(up = field_mice) %>%
##   bop(on = head)

## ----chain02, tidy = FALSE-----------------------------------------------
HELPrct %>% 
  select(ends_with("e")) %>% 
  head(2)
HELPrct %>% 
  select(starts_with("h")) %>% 
  head(2)
HELPrct %>% 
  select(matches("i[12]")) %>% # regex matching
  head(2)  

## ----names01-------------------------------------------------------------
names(faithful)

## ----names02-------------------------------------------------------------
names(faithful) <- c("duration", "time_til_next")
head(faithful, 3)

## ----data-revert, eval = TRUE--------------------------------------------
# don't execute this unless you want to revert to the original data
data(faithful)  

## ----rename01, fig.keep = "none"-----------------------------------------
data(faithful)
faithful <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithful %>% head(3)
gf_point(time_til_next ~ duration, data = faithful)

## ----rename01-fig--------------------------------------------------------
data(faithful)
faithful <- 
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithful %>% head(3)
gf_point(time_til_next ~ duration, data = faithful)

## ----rename02------------------------------------------------------------
data(CPS85)             # reload the data
CPS85 %>% 
  rename(education = educ) %>%
  head(4)  

## ----rename04, tidy = FALSE----------------------------------------------
CPS85 %>% 
  select(education = educ, wage, race) %>%
  head(3)

## ----filter01, fig.keep = "none"-----------------------------------------
# any logical can be used to create subsets
data(faithful)
faithful2 <-
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithfulLong <- 
  faithful2 %>%
  filter(duration > 3) 
gf_point(time_til_next ~ duration, data = faithfulLong)

## ----filter01-fig, echo = FALSE, results = "hide"------------------------
# any logical can be used to create subsets
data(faithful)
faithful2 <-
  faithful %>%
  rename(duration = eruptions, time_til_next = waiting)
faithfulLong <- 
  faithful2 %>%
  filter(duration > 3) 
gf_point(time_til_next ~ duration, data = faithfulLong)

## ----filter02, eval = FALSE, tidy = FALSE, fig.keep = "last", fig.show = "hide"----
## gf_point(time_til_next ~ duration,
##        data = faithful2 %>% filter( duration > 3))
## 
## # this one will use a different viewing window
## gf_point(time_til_next ~ duration, data = faithful2)  %>%
##   gf_lims(x = c(3, NA))
## 
## # Data can also be chained directly into ggformula functions
## faithful2 %>%
##   filter( duration > 3) %>%
##   gf_point(time_til_next ~ duration)

## ----summarise01---------------------------------------------------------
HELPrct %>% 
  summarise(x.bar = mean(age), s = sd(age))

## ----summarise02---------------------------------------------------------
HELPrct %>% 
  group_by(sex, substance) %>%
  summarise(x.bar = mean(age), s = sd(age))

## ----summarise03---------------------------------------------------------
favstats(age ~ sex + substance, data = HELPrct)
mean(age ~ sex + substance, data = HELPrct, .format = "table")
sd(age ~ sex + substance, data = HELPrct, .format = "table")

## ----arrange, tidy = FALSE-----------------------------------------------
HELPrct %>% 
  group_by(sex, substance) %>%
  summarise(x.bar = mean(age), s = sd(age)) %>% 
  arrange(x.bar)

## ----join01--------------------------------------------------------------
head(FUSION1, 3)
head(Pheno, 3)

## ----join02, tidy = FALSE------------------------------------------------
# merge FUSION1 and Pheno keeping only id's that are in both
FUSION1m <- merge(FUSION1, Pheno, by.x = "id", by.y = "id", 
                  all.x = FALSE, all.y = FALSE)
head(FUSION1m, 3)
left_join(Pheno, FUSION1, by = "id") %>% dim()
inner_join( Pheno, FUSION1, by = "id") %>% dim()
# which ids are only in Pheno?
setdiff(Pheno$id, FUSION1$id)   
anti_join(Pheno, FUSION1)

## ----join03--------------------------------------------------------------
tally( ~ t2d + genotype + marker, data = FUSION1m)

## ----merge-sol-----------------------------------------------------------
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

## ----spread01------------------------------------------------------------
Ut2 <-
  Utilities %>% 
  select(month, year, temp) 
Ut2 %>% head(3)

## ----spread02------------------------------------------------------------
require(tidyr) 
Ut3 <- 
  Ut2 %>%
  spread(key = month, value = temp)
Ut3 %>% head(4)

## ----gather01------------------------------------------------------------
Ut4 <- 
  Ut3 %>%
  gather(key = month, value = temp, `1` : `12`)  
Ut4 %>% head(4)

## ----gather02------------------------------------------------------------
Ut4a <- 
  Ut3 %>%
  gather(key = month, value = temp, 2 : 13)  
Ut4a %>% head(4)

## ----mode----------------------------------------------------------------
w <- 2.5; mode(w); length(w)
x <- c(1, 2); mode(x); length(x)
y <- "foo"; mode(y); length(y)
abc <- letters[1:3]
abc; mode(abc); length(abc)
z <- TRUE; mode(z); length(z)

## ----vectors01-----------------------------------------------------------
y[1]; y[2]             # not an error to ask for y[2]
abc[3]
abc[6] <- "Z"; abc     # NAs fill in to make vector long enough

## ----vectors02-----------------------------------------------------------
u <- c(first = 1, second = 2, third = 3)
u                                # names are displayed in output
names(u)                         # show just the names
u["second"]                      # access by name
names(u) <- c("one", "two", "three") # change the names
names(u)[2] <- "TWO"             # change just one name
u
u["first"]                       # old names gone now
setNames(u, c("a", "b", "c"))    # new object is named
u                                # u remains unchanged

## ----vectors03-----------------------------------------------------------
u * 3
sqrt(u)

## ----vectors04-----------------------------------------------------------
as.numeric(u)                    # creates nameless version
u                                # but doesn't change its input
names(u) <- NULL                 # this removes the names from u
u                                # no more names

## ----lists---------------------------------------------------------------
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

## ----attributes----------------------------------------------------------
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

## ----vectors05-----------------------------------------------------------
x <- 1:5; y <- seq(10, 60, by = 10); z <- rnorm(10); x; y
y + 1
x * 10
x < 3
x^2
log(x); log(x, base = 10)            # natural and base 10 logs

## ----vectors06-----------------------------------------------------------
# compare round() and signif() by binding rowwise into matrix
rbind(round(z, digits = 2), signif(z, digits = 2))   

## ----vectors07-----------------------------------------------------------
x <- 1:10; z <- rnorm(100)
mean(z); sd(z); var(z); median(z)  # basic statistical functions
range(z)                           # range returns a vector of length 2

## ----vectors08-----------------------------------------------------------
sum(x); prod(x)                         # sums and products
z <- rnorm(5); z
sort(z); rank(z); order(z)              # sort, rank, order
rev(x)                                  # reverse x
diff(x)                                 # pairwise differences
cumsum(x)                               # cumulative sum
cumprod(x)                              # cumulative product

## ----vectors09-----------------------------------------------------------
x <- 1:5; y <- seq(10, 70, by = 10)
x + y

## ----vectors10-----------------------------------------------------------
x <- seq(2, 20, by = 2)
x[1:5]; x[c(1, 4, 7)]

## ----vectors11-----------------------------------------------------------
x <- seq(2, 20, by = 2)
x[c(TRUE, TRUE, FALSE)]      # skips every third element (recycling!)
x[x > 10]                    # more typical use of boolean in selection

## ----vectors12-----------------------------------------------------------
x <- 1:10; x[-7]; x[-c(1, 2, 4, 8)]; x[-length(x)]

## ----vectors13-----------------------------------------------------------
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

## ----function02----------------------------------------------------------
fstats((1:20)^2)

## ----function03----------------------------------------------------------
fstats <- function(x) {
    print(mean(x))
    print(median(x))
    print(sd(x))
}

fstats((1:20)^2)

## ----function04----------------------------------------------------------
altfstats <- function(x) {
    cat(paste("  mean:", format(mean(x), 4), "\n"))
    cat(paste("median:", format(median(x), 4), "\n"))
    cat(paste("    sd:", format(sd(x), 4), "\n"))
}
altfstats((1:20)^2)

## ----function05----------------------------------------------------------
temp <- fstats((1:20)^2)
temp

## ----function06----------------------------------------------------------
fstats <- function(x) {
	c(mean(x), median(x), sd(x))
}
fstats((1:20)^2)

## ----function07----------------------------------------------------------
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

## ----gf-histogram-anatomy------------------------------------------------
gf_density()

## ----gf-bar-position, fig.keep = "none"----------------------------------
gf_bar()
gf_bar(~ substance, data = HELPrct, fill = ~ sex)
gf_bar(~ substance, data = HELPrct, fill = ~ sex, position = "dodge")

## ----gf-bar-position-fig, echo = FALSE, results = "hide", message = FALSE----
gf_bar()
gf_bar(~ substance, data = HELPrct, fill = ~ sex)
gf_bar(~ substance, data = HELPrct, fill = ~ sex, position = "dodge")

## ----gf-list-------------------------------------------------------------
apropos("^gf_")  # list all function that begin gf_

## ----facets, fig.keep = "none"-------------------------------------------
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ .)
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ ., scales = "free_y", space = "free")
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_wrap( ~ substance, ncol = 1)  
HELPrct %>% select(age, mcs, i1, cesd, substance) %>%
  gather(variable, value, age:cesd) %>%
  gf_dens( ~ value, color = ~substance) %>%
  gf_facet_wrap( ~ variable, scales = "free", ncol = 2)

## ----facets-fig, echo = FALSE--------------------------------------------
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ .)
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_grid( substance ~ ., scales = "free_y", space = "free")
gf_density( ~ age, data = HELPrct, fill = ~ sex) %>%
  gf_facet_wrap( ~ substance, ncol = 1)  
HELPrct %>% select(age, mcs, i1, cesd, substance) %>%
  gather(variable, value, age:cesd) %>%
  gf_dens( ~ value, color = ~substance) %>%
  gf_facet_wrap( ~ variable, scales = "free", ncol = 2)

## ----labs-theme, fig.keep = "none"---------------------------------------
gf_dens( ~ cesd, color = ~ substance, size = 1.5, data = HELPrct) %>%
  gf_labs(
    title = "Center for Epidemiologic Studies Depression measure",
    subtitle = "(at baseline)",
    color = "Abused substance: ",
    x = "CESD score",
    y = "",
    caption = "Source: HELPrct"
  ) %>%
  gf_theme(theme_classic()) %>%
  gf_theme(
    axis.text.y = element_blank(),
    legend.position = "top", 
    plot.title = element_text(hjust = 0.5, color = "navy"),
    plot.subtitle = element_text(hjust = 0.5, color = "navy", size = 12))

## ----labs-theme-fig, echo = FALSE, opts.label = "fig1"-------------------
gf_dens( ~ cesd, color = ~ substance, size = 1.5, data = HELPrct) %>%
  gf_labs(
    title = "Center for Epidemiologic Studies Depression measure",
    subtitle = "(at baseline)",
    color = "Abused substance: ",
    x = "CESD score",
    y = "",
    caption = "Source: HELPrct"
  ) %>%
  gf_theme(theme_classic()) %>%
  gf_theme(
    axis.text.y = element_blank(),
    legend.position = "top", 
    plot.title = element_text(hjust = 0.5, color = "navy"),
    plot.subtitle = element_text(hjust = 0.5, color = "navy", size = 12))

## ----scales-functions----------------------------------------------------
apropos("^scale_")

## ----scales, fig.keep = "none"-------------------------------------------
gf_point(length ~ width, data = KidsFeet, color = ~ sex) %>% 
  gf_lm() %>%
  gf_refine(scale_color_manual(values = c(B = "navy", G = "red")))
gf_bar(~ sex, fill = ~ substance, position = "dodge", data = HELPrct) %>%
  gf_refine(scale_fill_brewer(type = "qual", palette = 3))

## ----scales-fig, echo = FALSE--------------------------------------------
gf_point(length ~ width, data = KidsFeet, color = ~ sex) %>% 
  gf_lm() %>%
  gf_refine(scale_color_manual(values = c(B = "navy", G = "red")))
gf_bar(~ sex, fill = ~ substance, position = "dodge", data = HELPrct) %>%
  gf_refine(scale_fill_brewer(type = "qual", palette = 3))

## ----ggpairs, fig.show = "hide", message = FALSE-------------------------
GGally::ggpairs(iris)

## ----ggpairs-fig, echo = FALSE, message = FALSE, opts.label = "figbig"----
GGally::ggpairs(iris)

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

## ----echo = FALSE, opts.label = "fig1"-----------------------------------
require(weatherData, quietly = TRUE)
Temps <- NewYork2013 %>% mutate(city = "NYC") %>%
  bind_rows(Mumbai2013 %>% mutate(city = "Mumbai")) %>%
  bind_rows(London2013 %>% mutate(city = "London")) %>%
  mutate(date = lubridate::date(Time),
         month = lubridate::month(Time)) %>% 
  group_by(city, date) %>%
  summarise(
    hi = max(Temperature, na.rm = TRUE),
    lo = min(Temperature, na.rm = TRUE),
    mid = (hi + lo)/2
  )   
gf_linerange(lo + hi ~ date, color = ~ hi, data = Temps) %>%
  gf_facet_grid(city ~ .) %>%
  gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))


## ----MathNotation, child="MathNotation.Rnw", eval=includeApp[2]----------

## ----math-setup, include = FALSE, cache = FALSE--------------------------
require(fastR2)
knitr::opts_chunk$set(cache.path = "cache/Math-")

## ----sum-ssol------------------------------------------------------------
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

## ----LA-setup, include = FALSE-------------------------------------------
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

## ----projections01-sol---------------------------------------------------
a <- project(c(1, 0), c(1, 1)); a
b <- project(c(1, 0), c(1, -1)); b
c <- project(c(1, 0), c(1, 2)); c
d <- project(c(1, 2, 3), c(1, 1, 1)); d
e <- project(c(1, 1, 1), c(1, 2, 3)); fractions(e)
f <- project(c(1, 2, 3), c(1, -1, 0)); f
g <- project(c(1, 2, 3, 4), c(1, 1, -1, -1)); g
h <- project(c(1, 1, -1, -1), c(1, -1, 1, -1)); h

## ----projections02-sol---------------------------------------------------
a + b
a + c
d + f

## ----orthonormal-s1------------------------------------------------------
x <- c(1, 1, 1)
y <- c(1, 1, -2)
w <- y - project(y, x)
dot(x, w)                                    # confirm normality
# these two column vectors are orthogonal and have correct span
cbind( x / vlength(x), w / vlength(w) )

## ------------------------------------------------------------------------
x1 <- c(1, 1, 1)
x2 <- c(1, 2, 3)
u1 <- x1 / vlength(x1)
a <- x2 - project(x2, x1)
u2 <- a / vlength(a)
vlength(u1)
vlength(u2)
dot(u1, u2)

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

## ----rev-setup, include = FALSE------------------------------------------
knitr::opts_chunk$set(cache.path = "cache/Rev-")

## ----rev-data, fig.show='hide'-------------------------------------------
require(fastR2)
names(Batting)
Batting2005 <- Batting %>% filter(year == 2005)
df_stats(HR ~ team | league, data = Batting2005, max)

gf_histogram( ~ AB | league, data = Batting2005)
gf_point(HR ~ H, data = Batting2005 %>% filter(team == "DET"))
gf_boxplot(HR ~ league, data = Batting2005) %>%
  gf_refine(coord_flip())

## ----rev-data-fig, echo = FALSE, results = "hide"------------------------
require(fastR2)
names(Batting)
Batting2005 <- Batting %>% filter(year == 2005)
df_stats(HR ~ team | league, data = Batting2005, max)

gf_histogram( ~ AB | league, data = Batting2005)
gf_point(HR ~ H, data = Batting2005 %>% filter(team == "DET"))
gf_boxplot(HR ~ league, data = Batting2005) %>%
  gf_refine(coord_flip())

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


