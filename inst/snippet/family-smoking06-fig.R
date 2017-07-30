vcd::mosaic( ~ student + parents, 
             data = FamilySmoking %>%
               mutate(     # abbreviate labels to fit plot better
                 student = c("NS", "S")[as.numeric(student)],
                 parents = c("0", "1", "2")[as.numeric(parents)]
               ),
             shade = TRUE)

