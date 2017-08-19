vcd::mosaic(~ victim + defendant + death, 
            shade = TRUE,
            data = DeathPenalty %>%
              mutate(  # abbreviate labels to fit plot better
                victim = abbreviate(victim, 2),
                defendant = abbreviate(defendant, 2),
                death = abbreviate(death, 1))
)

