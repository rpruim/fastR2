bind_rows(
  lapply(
    levels(iris$Species),
    function(s) { 
      confint(t.test( ~ Sepal.Length / Sepal.Width, 
                      data = filter(iris, Species == s))) %>%
        mutate(species = s)
    }
  )
)

