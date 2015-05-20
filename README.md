<!-- README.md is generated from README.Rmd. Please edit that file -->
fastR
=====

This package contains data sets and some utility functions to support [*Foundations and Applications of Statistics: An Introduction Using R*](http://www.ams.org/bookstore-getitem/item=AMSTEXT-13) b Randall Pruim.

### Installation

The package can be installed from CRAN via

``` r
install.packages("fastR")
#> 
#> The downloaded binary packages are in
#>  /var/folders/qm/fvhzvrvx1qs1cr8c0pkk5f9m0000gn/T//Rtmp8iffHv/downloaded_packages
```

or from github

``` r
devtools::install_github("rpruim/fastR")
```

### Snippets

In addtion to data sets, `fastR` contains a `snippet()` function that loads and executes code found in the text. Here is an example:

``` r
require(fastR)
require(multcomp)
snippet("bugs")
#> 
#> 
#>  snippet(bugs)
#>  ------- ~~~~
#> 
#> > model <- aov(sqrt(NumTrap)~Color,bugs)
#> 
#> > TukeyHSD(model)
#>   Tukey multiple comparisons of means
#>     95% family-wise confidence level
#> 
#> Fit: aov(formula = sqrt(NumTrap) ~ Color, data = bugs)
#> 
#> $Color
#>          diff        lwr        upr     p adj
#> G-B  1.750330  0.6458303  2.8548288 0.0013396
#> W-B  0.146892 -0.9576072  1.2513913 0.9818933
#> Y-B  3.060201  1.9557018  4.1647003 0.0000011
#> W-G -1.603438 -2.7079368 -0.4989383 0.0031308
#> Y-G  1.309872  0.2053723  2.4143708 0.0165743
#> Y-W  2.913309  1.8088098  4.0178083 0.0000022
#> 
#> 
#> > model <- lm(sqrt(NumTrap)~Color,bugs)
#> 
#> > summary(glht(model,mcp(Color="Tukey")))
#> 
#>   Simultaneous Tests for General Linear Hypotheses
#> 
#> Multiple Comparisons of Means: Tukey Contrasts
#> 
#> 
#> Fit: lm(formula = sqrt(NumTrap) ~ Color, data = bugs)
#> 
#> Linear Hypotheses:
#>            Estimate Std. Error t value Pr(>|t|)    
#> G - B == 0   1.7503     0.3946   4.436  0.00130 ** 
#> W - B == 0   0.1469     0.3946   0.372  0.98190    
#> Y - B == 0   3.0602     0.3946   7.755  < 0.001 ***
#> W - G == 0  -1.6034     0.3946  -4.063  0.00296 ** 
#> Y - G == 0   1.3099     0.3946   3.319  0.01674 *  
#> Y - W == 0   2.9133     0.3946   7.383  < 0.001 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> (Adjusted p values reported -- single-step method)
```
