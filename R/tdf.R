#' @export
tdf <-
function (sd1, sd2, n1, n2) 
{
    v1 <- sd1^2/n1
    v2 <- sd2^2/n2
    df1 <- n1 - 1
    df2 <- n2 - 1
    num <- (v1 + v2)^2
    denom <- (v1^2/df1) + (v2^2/df2)
    return(num/denom)
}
