row.perc <-
function (x) 
{
    x/apply(x, 1, sum)
}
