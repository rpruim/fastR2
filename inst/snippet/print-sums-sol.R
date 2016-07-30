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

