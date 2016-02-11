football <- heliumFootballs      # give it a shorter name
head(football, 3)
 
football <- transform(football, diff = Helium - Air)
t.test( ~ diff, data=football)

