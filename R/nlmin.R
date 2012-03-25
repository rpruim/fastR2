nlmin <-
function (f, ...) 
{
    result <- nlm(f, ...)
    class(result) <- c("nlmin", class(result))
    return(result)
}
