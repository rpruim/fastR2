# Data from 
# Efficient Computing of Regression Diagnostics
# Author(s): Paul F. Velleman and Roy E. Welsch
# Source: The American Statistician, Vol. 35, No. 4 (Nov., 1981), pp. 234-242
# Published by: Taylor & Francis, Ltd. on behalf of the American Statistical Association
# Stable URL: http://www.jstor.org/stable/2683296
# Accessed: 20-08-2016 16:00 UTC

Unemployment <- 
  data.frame(
    unemp = c(3.1, 1.9, 1.7, 1.6, 3.2, 
          2.7, 2.6, 2.9, 4.7, 3.8),
    production = 
      c(113, 123, 127, 138, 130, 
        146, 151, 152, 141, 159),
    year = 1950:1959,
    iyear = 1:10
  )

devtools::use_data(Unemployment, overwrite = TRUE)

    