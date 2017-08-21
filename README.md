
<!-- README.md is generated from README.Rmd. Please edit that file -->




[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fastR2)](https://cran.r-project.org/package=fastR2)

fastR2
=======

This package contains data sets and some utility functions to support
*Foundations and Applications of Statistics: An Introduction Using R*
by Randall Pruim.

### Installation

The package can be installed from CRAN via

```r
install.packages("fastR2")
```
or from github

```r
devtools::install_github("rpruim/fastR2")
```

### Snippets

In addtion to data sets, `fastR2` contains a `snippet()` function that 
loads and executes code found in the text.  Here is an example:


```r
require(fastR2)
require(multcomp)
snippet("bugs")
#> 
#> ## snippet: bugs
#> 
#> > model <- aov(sqrt(trapped) ~ color, data = Bugs)
#> 
#> > TukeyHSD(model)
#>   Tukey multiple comparisons of means
#>     95% family-wise confidence level
#> 
#> Fit: aov(formula = sqrt(trapped) ~ color, data = Bugs)
#> 
#> $color
#>          diff        lwr        upr     p adj
#> G-B  1.750330  0.6458303  2.8548288 0.0013396
#> W-B  0.146892 -0.9576072  1.2513913 0.9818933
#> Y-B  3.060201  1.9557018  4.1647003 0.0000011
#> W-G -1.603438 -2.7079368 -0.4989383 0.0031308
#> Y-G  1.309872  0.2053723  2.4143708 0.0165743
#> Y-W  2.913309  1.8088098  4.0178083 0.0000022
#> 
#> 
#> > model <- lm(sqrt(trapped) ~ color, data = Bugs)
#> 
#> > glht(model, mcp(color = "Tukey")) %>%
#> +   summary()          
#> 
#> 	 Simultaneous Tests for General Linear Hypotheses
#> 
#> Multiple Comparisons of Means: Tukey Contrasts
#> 
#> 
#> Fit: lm(formula = sqrt(trapped) ~ color, data = Bugs)
#> 
#> Linear Hypotheses:
#>            Estimate Std. Error t value Pr(>|t|)    
#> G - B == 0   1.7503     0.3946   4.436  0.00136 ** 
#> W - B == 0   0.1469     0.3946   0.372  0.98189    
#> Y - B == 0   3.0602     0.3946   7.755  < 0.001 ***
#> W - G == 0  -1.6034     0.3946  -4.063  0.00312 ** 
#> Y - G == 0   1.3099     0.3946   3.319  0.01672 *  
#> Y - W == 0   2.9133     0.3946   7.383  < 0.001 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> (Adjusted p values reported -- single-step method)
#> 
#> 
#> ## snippet: fit-bugs-pois01
#> 
#> > o <- c(2, 10, 16, 11, 5, 3, 3)
#> 
#> > o.collapsed <- c(2 + 10, 16, 11, 5, 3 + 3)
#> 
#> > n <- sum(o)
#> 
#> > m <- sum(o * 0:6) / n     # mean count = MLE for lambda (full data)
#> 
#> > p <- dpois(0:6, m)  
#> 
#> > p.collapsed <- c(p[1] + p[2], p[3:5], 1 - sum(p[1:5]))   # collapsed probs
#> 
#> > e.collapsed <- p.collapsed * n
#> 
#> > cbind(o.collapsed, p.collapsed, e.collapsed)
#>      o.collapsed p.collapsed e.collapsed
#> [1,]          12   0.2752049   13.760244
#> [2,]          16   0.2533122   12.665609
#> [3,]          11   0.2161597   10.807986
#> [4,]           5   0.1383422    6.917111
#> [5,]           6   0.1169810    5.849050
#> 
#> > lrt  <- 2 * sum(o.collapsed * log(o.collapsed / e.collapsed)); lrt
#> [1] 1.640881
#> 
#> > pearson <- sum((o.collapsed - e.collapsed)^2 / e.collapsed); pearson
#> [1] 1.641642
#> 
#> > 1-pchisq(lrt, df = 3)
#> [1] 0.6501564
#> 
#> > 1-pchisq(pearson, df = 3)
#> [1] 0.6499852
#> 
#> ## snippet: fit-bugs-pois02
#> 
#> > 1-pchisq(pearson, df = 5-1)
#> [1] 0.8012892
#> 
#> > 1-pchisq(pearson, df = 5-1-1)
#> [1] 0.6499852
```


