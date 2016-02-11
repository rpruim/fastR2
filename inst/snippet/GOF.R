GOF <- function(x, 
    lik = function(theta, x) { 
        return(sum ( dnorm(x, mean = theta[1], sd = theta[2], log = TRUE) ) )
    } ,
    pdist = function(x, theta) { 
        return(pnorm(x, mean = theta[1], sd = theta[2]) ) 
    } , 
    start = c(0, 1), cutpts = quantile(x), 
    paramNames = paste('parameter', 1:length(p)),
    pearson = FALSE, ...) 
{
    ml <- maxLik(lik, start = start, x = x, ...)
    mle <- coef(ml)
    names(mle) <- paramNames
    prob <- diff( pdist(cutpts, mle))
    n <- length(x)
    o <- table(cut(x, cutpts))
    e <- prob * n


    pearsonStat <- sum( (o-e)^2 / e)
    lrtStat <- 2 * sum( o * log (o/e) )
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

    names(df) = 'df'
    names(stat) = "X-squared"
    message(returnMessage(ml))

    structure(list(
        nlmax = ml, statistic = stat, 
        estimate = coef(ml), parameter = df, 
        p.value = pval, method = method,
        data.name = deparse(substitute(x)), 
        observed = o, expected = e, 
        residuals = (o - e)/sqrt(e),
        table = cbind(o, e, prob),
        message = returnMessage(ml)
        ), 
        class = "htest")
}

