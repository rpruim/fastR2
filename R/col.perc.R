col.perc <-
function (x) 
{
    x/rep(apply(x, 2, sum), each = dim(x)[1])
}
