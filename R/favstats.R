favstats <-
function (x, na.rm = TRUE) 
{
    qq <- quantile(x, na.rm = na.rm)
    val <- c(qq, mean(x, na.rm = na.rm), sd(x, na.rm = na.rm), 
        var(x, na.rm = na.rm))
    names(val) <- c(names(qq), "mean", "sd", "var")
    return(val)
}
