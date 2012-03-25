project <-
function (x, u = rep(1, length(x)), type = c("vector", "length")) 
{
    type = match.arg(type)
    switch(type, vector = u * (dot(x, u)/dot(u, u)), length = dot(x, 
        u)/sqrt(dot(u, u)), )
}
