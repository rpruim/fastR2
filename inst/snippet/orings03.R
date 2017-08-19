# by default, makeFun() uses type = "response" and 
# returns values on data scale
temp2damage <- makeFun(orings.model) 
temp2damage(temp = 31) 
makeFun(orings.model)(31)   # We can do it all in one line if we prefer
# the other option is type = "link"
temp2damage <- makeFun(orings.model, type = "link")
temp2damage(temp = 31)

