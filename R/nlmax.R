nlmax <-
function (f, ...) 
{
    g <- function(...) {
        -f(...)
    }
    result <- nlm(g, ...)
    result$minimum <- -result$minimum
    names(result)[1] <- "maximum"
    class(result) <- c("nlmax", class(result))
    return(result)
}
