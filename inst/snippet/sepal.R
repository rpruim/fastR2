for (species in levels(iris$Species)) { 
    print( confint( t.test(~Sepal.Length/Sepal.Width, 
						   data=subset(iris, Species==species)) ) )
}

